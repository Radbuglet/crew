use bumpalo::Bump;
use std::alloc::{AllocError, Allocator, Layout};
use std::fmt::{Debug, Formatter};
use std::hash::{Hash, Hasher};
use std::num::NonZeroUsize;
use std::ptr::NonNull;
use std::rc::Rc;
use std::sync::atomic::{AtomicUsize, Ordering};

static UID: AtomicUsize = AtomicUsize::new(0);

// N.B. `BumpAlloc` does not implement `Default` because constructors might build a new default
// allocator if the arena is not passed explicitly, leading to accidental performance problems.
#[derive(Clone)]
pub struct BumpAlloc {
    rc: Rc<AllocInner>,
}

struct AllocInner {
    bump: Bump,
    uid: NonZeroUsize,
}

impl BumpAlloc {
    pub fn new() -> Self {
        Self::new_with(Default::default())
    }

    pub fn new_with(bump: Bump) -> Self {
        Self {
            rc: Rc::new(AllocInner {
                bump,
                uid: NonZeroUsize::new(UID.fetch_add(1, Ordering::Relaxed))
                    .expect("BumpAlloc UID overflowed!"),
            }),
        }
    }

    pub fn uid(&self) -> NonZeroUsize {
        self.rc.uid
    }
}

impl Eq for BumpAlloc {}

impl PartialEq for BumpAlloc {
    fn eq(&self, other: &Self) -> bool {
        self.rc.uid == other.rc.uid
    }
}

impl Hash for BumpAlloc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.rc.uid.get());
    }
}

impl Debug for BumpAlloc {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BumpAlloc")
            .field("uid", &self.uid().get())
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
