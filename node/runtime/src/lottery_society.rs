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
				Self::deposit_event(RawEvent::MemberAdded(joiner));
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
				native::info!("LOTTERY: not executing on_finalize");
				return;
			}

			if (n % T::PayoutPeriod::get()).is_zero() {
				Self::run_lottery(1000.into());
			}

		}

		// transfer tokens from one account to another
		// fn transfer(origin, to: T::AccountId, value: u64) -> DispatchResult {
		// 	let sender = ensure_signed(origin)?;
		// 	let sender_balance = Self::balance_of(sender.clone());
		// 	ensure!(sender_balance >= value, "Not enough balance.");

		// 	let updated_from_balance = sender_balance.checked_sub(value).ok_or("overflow in calculating balance")?;
		// 	let receiver_balance = Self::balance_of(to.clone());
		// 	let updated_to_balance = receiver_balance.checked_add(value).ok_or("overflow in calculating balance")?;

		// 	// reduce sender's balance
		// 	<BalanceOf<T>>::insert(sender, updated_from_balance);

		// 	// increase receiver's balance
		// 	<BalanceOf<T>>::insert(to.clone(), updated_to_balance);

		// 	Ok(())
		// }

		// fn add_member(origin) -> DispatchResult {
		// 	let who = ensure_signed(origin)?;

		// 	// Note: We use a 1-based (instead of 0-based) list here
		// 	// Note: Handle overflow here in production code!
		// 	let new_count = <LargestIndex>::get() + 1;
		// 	// insert new member past the end of the list
		// 	<TheList<T>>::insert(new_count, &who);
		// 	// store the incremented count
		// 	<LargestIndex>::put(new_count);

		// 	Self::deposit_event(RawEvent::MemberAdded(who));

		// 	Ok(())
		// }
	}
}

impl<T: Trait> Module<T> {
	pub fn account_id() -> T::AccountId {
		MODULE_ID.into_account()
	}

	pub fn is_member(acc: &T::AccountId) -> bool {
		Self::members().contains(acc)
	}

	fn run_lottery(fraction: BalanceOf<T>) {
		let pot_account = Self::account_id();
		let pot = T::Currency::free_balance(&pot_account);

		assert!(
			fraction > 1.into(),
			"only allow drawing out less than the whole pot"
		);
		let lottery_amount = pot
			.checked_div(&fraction)
			.expect("should always be possible for any value of the balance");

		if lottery_amount < 100.into() {
			native::info!("LOTTERY: lottey too small, skipping round");
			return;
		}

		let nonce = <Nonce>::get();
		let seed = T::Randomness::random_seed();
		let random_num = match (seed, nonce)
			.using_encoded(|b| T::Hashing::hash(b))
			.using_encoded(|mut b| u64::decode(&mut b))
			.map_err(|_| "randomness failed")
		{
			Ok(num) => num,
			Err(_) => return,
		};

		let members = <Members<T>>::get();
		assert!(
			members.len() > 0,
			"there needs to be at least 1 member to the lottery"
		);

		let index = (random_num % (members.len() as u64)) as usize;
		native::info!("random: {}, random_index: {}", random_num, index);
		let random_member = &members[index];

		if T::Currency::transfer(&pot_account, random_member, lottery_amount, KeepAlive).is_ok() {
			Self::deposit_event(RawEvent::Winner(random_member.clone(), lottery_amount));
		};
	}
}
