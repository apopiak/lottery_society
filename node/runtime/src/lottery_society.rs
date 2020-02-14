use frame_support::{
	debug::{self, native},
	decl_event, decl_module, decl_storage, dispatch, ensure,
	traits::{Currency, ExistenceRequirement::KeepAlive, Get, Randomness},
};
use sp_core::{Decode, Encode};
use sp_runtime::{
	traits::{
		AccountIdConversion, CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, Hash,
		SimpleArithmetic, Zero,
	},
	DispatchResult, ModuleId,
};
use sp_std::prelude::*;
use system::ensure_signed;

type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

const MODULE_ID: ModuleId = ModuleId(*b"lot_soci");

// the module configuration trait
pub trait Trait: system::Trait {
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	type Currency: Currency<Self::AccountId>;
	type Randomness: Randomness<Self::Hash>;

	type ExistentialDeposit: Get<BalanceOf<Self>>;
	type MinimumPayout: Get<BalanceOf<Self>>;

	/// The number of blocks between payout periods.
	type PayoutPeriod: Get<Self::BlockNumber>;
}

decl_event!(
	pub enum Event<T>
	where
		AccountId = <T as system::Trait>::AccountId,
		Balance = BalanceOf<T>,
	{
		Founded(AccountId, Balance),
		MemberAdded(AccountId),
		MemberRemoved(AccountId),
		Winner(AccountId, Balance),
	}
);

// storage for this runtime module
decl_storage! {
	trait Store for Module<T: Trait> as LotterySociety {
		Founder get(fn founder): T::AccountId;

		Members get(fn members): Vec<T::AccountId>;

		PayingMembers get(fn paying_members): Vec<T::AccountId>;

		// Pot get(fn pot): BalanceOf<T>;

		// TotalSupply get(fn total_supply) config(): u64 = 21000000;

		// BalanceOf get(fn balance_of): map hasher(blake2_256) T::AccountId => u64;

		// TheList: map hasher(blake2_256) u32 => T::AccountId;
		// LargestIndex: u32;

		// TheLinkedList: linked_map hasher(blake2_256) u32 => T::AccountId;

		Nonce: u64;
	}
}

// public interface for this runtime module
decl_module! {
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event() = default;

		// initialize the society
		// transfers initial pot amount of currency to the society
		pub fn found(origin, initial_pot: BalanceOf<T>) -> DispatchResult {
			let founder = ensure_signed(origin)?;

			ensure!(Self::members().is_empty(), "can only found the society if it does not exist == no members");

			let _ = T::Currency::transfer(&founder, &Self::account_id(), initial_pot, KeepAlive)?;
			<Members<T>>::put(vec![&founder]);
			<Founder<T>>::put(&founder);

			Self::deposit_event(RawEvent::Founded(founder, initial_pot));

			Ok(())
		}

		pub fn enter_lottery(origin, bid: BalanceOf<T>) -> DispatchResult {
			let joiner = ensure_signed(origin)?;

			let _ = T::Currency::transfer(&joiner, &Self::account_id(), bid, KeepAlive)?;

			if !Self::is_member(&joiner) {
				<Members<T>>::append(vec![&joiner])?;
				Self::deposit_event(RawEvent::MemberAdded(joiner.clone()));
			}

			if !Self::paying_members().contains(&joiner) {
				<PayingMembers<T>>::append(vec![&joiner])?;
			}

			Ok(())
		}

		pub fn trigger_lottery(origin) -> DispatchResult {
			let founder = ensure_signed(origin)?;

			ensure!(founder == Self::founder(), "only the founder can trigger the lottery");

			Self::run_lottery(10.into());

			Ok(())
		}

		fn on_initialize(n: T::BlockNumber) {
			if Self::founder() == T::AccountId::default() {
				native::info!("LOTTERY: no founder --> not initialized --> not executing");
				return;
			}

			if (n % T::PayoutPeriod::get()).is_zero() {
				Self::kick_non_paying_member();
				Self::run_lottery(100.into());
				<PayingMembers<T>>::put(Vec::<T::AccountId>::new());
			}
		}
	}
}

impl<T: Trait> Module<T> {
	pub fn account_id() -> T::AccountId {
		MODULE_ID.into_account()
	}

	pub fn is_member(acc: &T::AccountId) -> bool {
		Self::members().contains(acc)
	}

	fn kick_non_paying_member() {
		let mut members = Self::members();
		let paying = Self::paying_members();
		let non_paying: Vec<(usize, &T::AccountId)> = members
			.iter()
			.enumerate()
			.filter(|(i, m)| !paying.contains(m))
			.collect();
		Self::random_index(non_paying.len())
			.map(|i| non_paying[i].0)
			.map(|i| {
				members.remove(i);
				<Members<T>>::put(members);
			});
	}

	fn run_lottery(fraction: BalanceOf<T>) {
		let pot_account = Self::account_id();
		let pot = T::Currency::free_balance(&pot_account);

		if pot < (T::ExistentialDeposit::get() * 10.into()).into() {
			native::info!("LOTTERY: pot too small, skipping payout");
			return;
		}

		assert!(
			fraction > 1.into(),
			"only allow drawing out less than the whole pot"
		);
		let lottery_amount = pot
			.checked_div(&fraction)
			.expect("should always be possible for any value of the balance");

		if lottery_amount < T::MinimumPayout::get() {
			native::info!("LOTTERY: payout too small, skipping round");
			return;
		}

		let members = Self::members();
		let random_member = Self::random_member(&members);
		let random_member = match random_member {
			Ok(member) => member,
			Err(msg) => {
				native::info!("LOTTERY: choosing random member failed, {}", msg);
				return;
			}
		};

		native::info!(
			"LOTTERY: Winner Winner, chicken dinner! {:?}, {:?}",
			random_member.clone(),
			lottery_amount
		);
		if T::Currency::transfer(&pot_account, &random_member, lottery_amount, KeepAlive).is_ok() {
			Self::deposit_event(RawEvent::Winner(random_member.clone(), lottery_amount));
		};
	}

	fn random_member(members: &[T::AccountId]) -> Result<T::AccountId, &'static str> {
		let index = Self::random_index(members.len())?;
		Ok(members[index].clone())
	}

	fn random_index(len: usize) -> Result<usize, &'static str> {
		ensure!(len > 0, "need more than 1 option for random index");

		let nonce = <Nonce>::get();
		let seed = T::Randomness::random_seed();
		let random_num = (seed, nonce)
			.using_encoded(|b| T::Hashing::hash(b))
			.using_encoded(|mut b| u64::decode(&mut b))
			.map_err(|_| "randomness failed")?;

		<Nonce>::put(nonce + 1);

		Ok((random_num % (len as u64)) as usize)
	}
}
