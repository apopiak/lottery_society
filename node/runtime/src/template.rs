use frame_support::{decl_module, decl_storage, decl_event, dispatch, ensure};
use system::ensure_signed;
use sp_runtime::{
	DispatchResult,
	traits::{
		SimpleArithmetic, CheckedAdd, CheckedSub,
	},
};

// the module configuration trait
pub trait Trait: system::Trait {
  type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_event!(
  pub enum Event<T> where AccountId = <T as system::Trait>::AccountId {
    MemberAdded(AccountId),
    MemberRemoved(AccountId),
  }
);

// storage for this runtime module
decl_storage! {
  trait Store for Module<T: Trait> as Template {
    TotalSupply get(fn total_supply) config(): u64 = 21000000;

    BalanceOf get(fn balance_of): map hasher(blake2_256) T::AccountId => u64;

    TheList: map hasher(blake2_256) u32 => T::AccountId;
    LargestIndex: u32;

    TheLinkedList: linked_map hasher(blake2_256) u32 => T::AccountId;
  }
}

// public interface for this runtime module
decl_module! {
  pub struct Module<T: Trait> for enum Call where origin: T::Origin {
      fn deposit_event() = default;

      // initialize the token
      // transfers the total_supply amout to the caller
      fn init(origin) -> DispatchResult {
        let sender = ensure_signed(origin)?;
        <BalanceOf<T>>::insert(sender, Self::total_supply());
        Ok(())
      }

      // transfer tokens from one account to another
      fn transfer(origin, to: T::AccountId, value: u64) -> DispatchResult {
        let sender = ensure_signed(origin)?;
        let sender_balance = Self::balance_of(sender.clone());
        ensure!(sender_balance >= value, "Not enough balance.");

        let updated_from_balance = sender_balance.checked_sub(value).ok_or("overflow in calculating balance")?;
        let receiver_balance = Self::balance_of(to.clone());
        let updated_to_balance = receiver_balance.checked_add(value).ok_or("overflow in calculating balance")?;
        
        // reduce sender's balance
        <BalanceOf<T>>::insert(sender, updated_from_balance);

        // increase receiver's balance
        <BalanceOf<T>>::insert(to.clone(), updated_to_balance);
        
        Ok(())
      }

      fn add_member(origin) -> DispatchResult {
        let who = ensure_signed(origin)?;
    
        // Note: We use a 1-based (instead of 0-based) list here
        // Note: Handle overflow here in production code!
        let new_count = <LargestIndex>::get() + 1;
        // insert new member past the end of the list
        <TheList<T>>::insert(new_count, &who);
        // store the incremented count
        <LargestIndex>::put(new_count);
    
        Self::deposit_event(RawEvent::MemberAdded(who));
    
        Ok(())
      }

      fn remove_member_discontiguous(origin, index: u32) -> DispatchResult {
        let _ = ensure_signed(origin)?;
    
        // verify existence
        ensure!(<TheList<T>>::exists(index), "an element doesn't exist at this index");
        // use take for event emission, use remove to drop value
        let removed_member = <TheList<T>>::take(index);
    
        Self::deposit_event(RawEvent::MemberRemoved(removed_member));
    
        Ok(())
      }

      fn remove_member_contiguous(origin, index: u32) -> DispatchResult {
        let _ = ensure_signed(origin)?;
    
        ensure!(<TheList<T>>::exists(index), "an element doesn't exist at this index");
    
        let largest_index = <LargestIndex>::get();
        // swap
        if index != largest_index {
            <TheList<T>>::swap(index, largest_index);
        }
        // pop, uses `take` to return the member in the event
        let removed_member = <TheList<T>>::take(largest_index);
        <LargestIndex>::mutate(|count| *count - 1);
    
        Self::deposit_event(RawEvent::MemberRemoved(removed_member));
    
        Ok(())
      }

      fn remove_member_linked(origin, index: u32) -> DispatchResult {
        let _ = ensure_signed(origin)?;
    
        ensure!(<TheLinkedList<T>>::exists(index), "A member does not exist at this index");
        let removed_member = <TheLinkedList<T>>::take(index);

        Self::deposit_event(RawEvent::MemberRemoved(removed_member));
    
        Ok(())
      }
  }
}