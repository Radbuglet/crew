use std::alloc::{AllocError, Allocator, Global, Layout};
use std::cell::{Cell, UnsafeCell};
use std::marker::{PhantomData, Unsize};
use std::ops::{CoerceUnsized, Deref};
use std::ptr::NonNull;
use std::{mem, ptr};

pub const OOM_MSG: &'static str = "out of memory";

pub struct CustomRc<T: ?Sized, A: Allocator = Global> {
    _ty: PhantomData<T>,
    inner: NonNull<RcInner<T, A>>,
}

impl<T, U, A> CoerceUnsized<CustomRc<U, A>> for CustomRc<T, A>
where
    T: ?Sized + Unsize<U>,
    U: ?Sized,
    A: Allocator,
{
}

impl<T, A: Allocator> CustomRc<T, A> {
    pub fn new(value: T) -> Self
    where
        A: Default,
    {
        Self::new_in(value, A::default())
    }

    pub fn new_in(value: T, alloc: A) -> Self {
        Self::try_new_in(value, alloc).expect(OOM_MSG)
    }

    pub fn try_new_in(value: T, alloc: A) -> Result<Self, AllocError> {
        let inner = alloc
            .allocate(Layout::new::<RcInner<T, A>>())?
            .cast::<RcInner<T, A>>();

        unsafe {
            inner.as_ptr().write(RcInner {
                strong: Cell::new(1),
                weak: Cell::new(0),
                alloc,
                value: UnsafeCell::new(value),
            });
        }

        Ok(Self {
            _ty: PhantomData,
            inner,
        })
    }

    pub fn try_unwrap(rc: Self) -> Result<T, Self> {
        if rc.inner().strong() == 1 {
            // Copy value to stack, invalidating the heap copy.
            let inner = unsafe { ptr::read(rc.inner().value.get()) };

            // Decrement the strong count and potentially deallocate with running drop.
            Self::dec_strong_maybe_dealloc(&rc);

            // Ensure that we don't run the smart pointer's destructor, which assumes that we're
            // still upholding validity invariants.
            mem::forget(rc);

            Ok(inner)
        } else {
            Err(rc)
        }
    }

    pub fn unwrap(rc: Self) -> T {
        match Self::try_unwrap(rc) {
            Ok(value) => value,
            Err(rc) => panic!(
                "Cannot unwrap Rc with a non-unique strong reference! Found {} strong references.",
                rc.inner().strong(),
            ),
        }
    }
}

impl<T: Clone, A: Clone + Allocator> CustomRc<T, A> {
    pub fn make_mut(rc: &mut Self) -> &mut T {
        // Make the RC unique
        if rc.inner().strong() != 1 {
            // This RC has multiple strong references so our only option is to clone the entire value.
            *rc = Self::owning_clone(rc);
        } else if rc.inner().weak() != 0 {
            // We can steal the value from this RC and condemn it, avoiding a call to clone. We don't
            // have to check for weak freeing conditions because we know there's at least one weak.
            let value = unsafe { ptr::read(rc.inner().value_raw()) };
            rc.inner().dec_strong();

            // Move the value into a new RC
            *rc = Self::new_in(value, rc.inner().alloc.clone());
        } else {
            // (the RC is already unique)
        }

        // Fetch the value
        debug_assert!(Self::is_unique(rc));
        unsafe { rc.inner().value_mut() }
    }

    pub fn try_owning_clone(rc: &Self) -> Result<Self, AllocError> {
        Self::try_new_in((&**rc).clone(), rc.inner().alloc.clone())
    }

    pub fn owning_clone(rc: &Self) -> Self {
        Self::try_owning_clone(rc).expect(OOM_MSG)
    }
}

impl<T: ?Sized, A: Allocator> CustomRc<T, A> {
    pub fn is_unique(rc: &Self) -> bool {
        Self::strong_count(rc) == 1 && Self::weak_count(rc) == 0
    }

    pub fn strong_count(rc: &Self) -> usize {
        rc.inner().strong()
    }

    pub fn weak_count(rc: &Self) -> usize {
        rc.inner().weak()
    }

    pub fn downgrade(rc: &Self) -> CustomWeak<T, A> {
        rc.inner().inc_weak();
        CustomWeak {
            _ty: PhantomData,
            inner: rc.inner,
        }
    }

    pub fn try_get_mut(rc: &mut Self) -> Option<&mut T> {
        if Self::is_unique(rc) {
            Some(unsafe { rc.inner().value_mut() })
        } else {
            None
        }
    }

    pub fn get_mut(rc: &mut Self) -> &mut T {
        let strong = rc.inner().strong();
        let weak = rc.inner().weak();

        Self::try_get_mut(rc).expect(
            format!(
                "Cannot get unique mutable reference to Rc: Rc is being referenced by {} strong \
                reference{} and {} weak reference{} but must be referenced by exactly one strong reference.",
                strong,
                if strong == 1 { "" } else { "s" },
                weak,
                if weak == 1 { "" } else { "s" },
            ).as_str(),
        )
    }

    pub fn get_raw(rc: &Self) -> &UnsafeCell<T> {
        &rc.inner().value
    }

    pub fn ptr_eq<T2: ?Sized, A2: Allocator>(a: &Self, b: &CustomRc<T2, A2>) -> bool {
        a.inner.cast::<()>() == b.inner.cast::<()>()
    }

    fn dec_strong_maybe_dealloc(&self) {
        self.inner().dec_strong();
        if self.inner().weak() == 0 {
            unsafe {
                dealloc_rc_inner(self.inner);
            }
        }
    }

    fn inner(&self) -> &RcInner<T, A> {
        unsafe { self.inner.as_ref() }
    }
}

impl<T: ?Sized, A: Allocator> Clone for CustomRc<T, A> {
    fn clone(&self) -> Self {
        self.inner().inc_strong();
        Self {
            _ty: PhantomData,
            inner: self.inner,
        }
    }
}

impl<T, A: Allocator> Deref for CustomRc<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { self.inner().value() }
    }
}

impl<T: ?Sized, A: Allocator> Drop for CustomRc<T, A> {
    fn drop(&mut self) {
        // Drop the value if we're the only strong reference left.
        if self.inner().strong() == 1 {
            unsafe { ptr::drop_in_place(self.inner().value_raw()) }
        }

        // Decrement the strong count and deallocate if applicable.
        Self::dec_strong_maybe_dealloc(self);
    }
}

pub struct CustomWeak<T: ?Sized, A: Allocator = Global> {
    _ty: PhantomData<T>,
    inner: NonNull<RcInner<T, A>>,
}

impl<T: ?Sized, A: Allocator> CustomWeak<T, A> {
    pub fn strong_count(&self) -> usize {
        self.inner().strong()
    }

    pub fn weak_count(&self) -> usize {
        self.inner().weak()
    }

    pub fn can_upgrade(&self) -> bool {
        self.inner().strong() > 0
    }

    pub fn try_upgrade(&self) -> Option<CustomRc<T, A>> {
        if self.can_upgrade() {
            Some(CustomRc {
                _ty: PhantomData,
                inner: self.inner,
            })
        } else {
            None
        }
    }

    pub fn upgrade(&self) -> CustomRc<T, A> {
        self.try_upgrade().unwrap()
    }

    pub fn ptr_eq<T2: ?Sized, A2: Allocator>(&self, b: &Self) -> bool {
        self.inner.cast::<()>() == b.inner.cast::<()>()
    }

    fn inner(&self) -> &RcInner<T, A> {
        unsafe { self.inner.as_ref() }
    }
}

impl<T: ?Sized, A: Allocator> Drop for CustomWeak<T, A> {
    fn drop(&mut self) {
        self.inner().dec_weak();

        // Any remaining strong references will already have dropped the RC's contents, all we need
        // to do is deallocate the memory if no smart pointers remain.
        if self.inner().strong() == 0 && self.inner().weak() == 0 {
            unsafe { dealloc_rc_inner(self.inner) }
        }
    }
}

unsafe fn dealloc_rc_inner<T: ?Sized, A: Allocator>(ptr: NonNull<RcInner<T, A>>) {
    // Move the allocator to the stack, invalidating the heap copy.
    let alloc = ptr::read(&ptr.as_ref().alloc);

    // Free the heap allocation.
    alloc.deallocate(ptr.cast::<u8>(), Layout::for_value(ptr.as_ref()));
}

// N.B. Both `alloc` and `value` may become logically uninitialized because we either manually drop
// them or move them.
struct RcInner<T: ?Sized, A: Allocator> {
    strong: Cell<usize>,
    weak: Cell<usize>,
    alloc: A,
    value: UnsafeCell<T>,
}

impl<T: ?Sized, A: Allocator> RcInner<T, A> {
    pub fn inc_strong(&self) {
        debug_assert!(self.strong.get() > 0);
        self.strong.set(
            self.strong.get().checked_add(1).expect(
                format!(
                    "Cannot create more than {} RCs to the same value!",
                    usize::MAX
                )
                .as_str(),
            ),
        );
    }

    pub fn dec_strong(&self) {
        debug_assert!(self.strong.get() > 0);
        self.strong.set(self.strong.get() - 1);
    }

    pub fn inc_weak(&self) {
        debug_assert!(self.weak.get() > 0);
        self.weak.set(
            self.weak.get().checked_add(1).expect(
                format!(
                    "Cannot create more than {} weak RCs to the same value!",
                    usize::MAX
                )
                .as_str(),
            ),
        );
    }

    pub fn dec_weak(&self) {
        debug_assert!(self.weak.get() > 0);
        self.weak.set(self.weak.get() - 1);
    }

    pub fn strong(&self) -> usize {
        self.strong.get()
    }

    pub fn weak(&self) -> usize {
        self.weak.get()
    }

    pub unsafe fn value(&self) -> &T {
        &*self.value.get()
    }

    pub unsafe fn value_mut(&self) -> &mut T {
        &mut *self.value.get()
    }

    pub fn value_raw(&self) -> *mut T {
        self.value.get()
    }
}
