use bumpalo::Bump;
use std::alloc::{AllocError, Allocator, Layout};
use std::cell::Cell;
use std::ptr::NonNull;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct BumpAlloc {
    inner: Rc<BumpAllocInner>,
}

#[derive(Debug)]
struct BumpAllocInner {
    bump: Bump,
    allocs: Cell<usize>,
}

impl Default for BumpAlloc {
    fn default() -> Self {
        Self::new()
    }
}

impl BumpAlloc {
    pub fn new() -> Self {
        Self::new_with(Default::default())
    }

    pub fn new_with(bump: Bump) -> Self {
        Self {
            inner: Rc::new(BumpAllocInner {
                bump,
                allocs: Cell::new(0),
            }),
        }
    }

    fn inc_allocs(&self) {
        // TODO: Remove overflow checks
        // We could remove overflow checks if tracked the number of allocated bytes instead of the
        // number of potentially-zero-sized allocations but handling this in a spec-compliant way
        // seems difficult.
        self.inner.allocs.set(
            self.inner
                .allocs
                .get()
                .checked_add(1)
                .expect("too many concurrent allocations"),
        )
    }

    fn dec_allocs(&self) {
        // Valid use of the Allocator means that there must be at most one deallocation for each
        // allocation so this number should never go negative in regular use.
        self.inner.allocs.set(self.inner.allocs.get() - 1)
    }
}

unsafe impl Allocator for BumpAlloc {
    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        self.inc_allocs();
        (&self.inner.bump).allocate(layout)
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        self.dec_allocs();
        (&self.inner.bump).deallocate(ptr, layout)
    }

    unsafe fn grow(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.inner.bump).grow(ptr, old_layout, new_layout)
    }

    unsafe fn grow_zeroed(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.inner.bump).grow_zeroed(ptr, old_layout, new_layout)
    }

    unsafe fn shrink(
        &self,
        ptr: NonNull<u8>,
        old_layout: Layout,
        new_layout: Layout,
    ) -> Result<NonNull<[u8]>, AllocError> {
        (&self.inner.bump).shrink(ptr, old_layout, new_layout)
    }
}

impl Drop for BumpAlloc {
    fn drop(&mut self) {
        if self.inner.allocs.get() > 0 {
            std::mem::forget(self.inner.clone());
            log::warn!("Dropped BumpAlloc before all allocations were freed, causing the entire slab to be leaked.");
        }
    }
}
