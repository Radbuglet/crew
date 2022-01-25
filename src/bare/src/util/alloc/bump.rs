use bumpalo::Bump;
use std::alloc::{AllocError, Allocator, Layout};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;
use std::rc::Rc;

// N.B. `BumpAlloc` does not implement `Default` because constructors might build a new default
// allocator if the arena is not passed explicitly, leading to accidental performance problems.
#[derive(Clone)]
pub struct BumpAlloc {
    rc: Rc<AllocInner>,
}

struct AllocInner {
    bump: Bump,
}

impl BumpAlloc {
    pub fn new() -> Self {
        Self::new_with(Default::default())
    }

    pub fn new_with(bump: Bump) -> Self {
        Self {
            rc: Rc::new(AllocInner { bump }),
        }
    }

    pub fn assert_finalize(self) {
        let refs = Rc::strong_count(&self.rc) - 1;
        if refs > 0 {
            log::warn!(
                "Finalization barrier could not be upheld. BumpAlloc still has {} allocator handle{}!",
                refs,
                if refs == 1 { "" } else { "s" }
            );
        }
    }
}

impl Eq for BumpAlloc {}

impl PartialEq for BumpAlloc {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.rc, &other.rc)
    }
}

impl Hash for BumpAlloc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(Rc::as_ptr(&self.rc) as usize);
    }
}

impl Debug for BumpAlloc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BumpAlloc")
            .field("rc", &Rc::as_ptr(&self.rc))
            .finish()
    }
}

// N.B. Fun fact: allocators are allowed to free their reservations if all allocators go out of scope
// so this code is valid without needing any additional byte tracking.
unsafe impl Allocator for BumpAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        (&self.rc.bump).allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        (&self.rc.bump).deallocate(ptr, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.rc.bump).grow(ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.rc.bump).grow_zeroed(ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.rc.bump).shrink(ptr, old_layout, new_layout)
    }
}
