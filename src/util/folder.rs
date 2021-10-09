// === Generic === //

use crate::util::slice_ext::ArrayCollectExt;
use std::mem::{transmute, MaybeUninit};

/// Folders a very useful reader-like construct which allow users to procedurally move through an
/// array from either left-to-right ([RightFolder]) or right-to-left ([LeftFolder]) to accept, replace,
/// and skip elements from the array to reduce it.
pub trait Folder {
    type Item;

    // === List querying === //

    fn remaining(&self) -> usize;

    fn has_remaining(&self) -> bool {
        self.remaining() > 0
    }

    fn is_fully_processed(&self) -> bool {
        self.remaining() == 0
    }

    fn split(&mut self) -> (&mut [Self::Item], &mut [Self::Item]);

    fn preceding(&mut self) -> &mut [Self::Item] {
        self.split().0
    }

    fn proceeding(&mut self) -> &mut [Self::Item] {
        self.split().1
    }

    // === Primitive list modification === //

    fn new_len(&self) -> usize;

    fn commit_many(&mut self, count: usize);

    fn commit(&mut self) {
        self.commit_many(1);
    }

    fn omit_many(&mut self, count: usize);

    fn omit(&mut self) {
        self.omit_many(1);
    }

    fn take(&mut self) -> Self::Item;

    fn take_many(&mut self, count: usize) -> FolderOmitDrain<'_, Self> {
        FolderOmitDrain::new(self, count)
    }

    // === Higher-order list modification === //

    fn produce(&mut self, count: usize, value: Self::Item) {
        self.reduce(count, value);
        self.commit();
    }

    fn reduce(&mut self, count: usize, value: Self::Item) {
        debug_assert!(self.remaining() >= count);

        self.proceeding()[count - 1] = value;
        self.omit_many(count - 1);
    }
}

pub struct FolderOmitDrain<'a, T: ?Sized> {
    target: &'a mut T,
    remaining: usize,
}

impl<'a, T: ?Sized> FolderOmitDrain<'a, T> {
    pub fn new(target: &'a mut T, count: usize) -> Self {
        Self {
            target,
            remaining: count,
        }
    }
}

impl<T: ?Sized + Folder> Iterator for FolderOmitDrain<'_, T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining > 0 {
            self.remaining -= 1;
            Some(self.target.take())
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<T: ?Sized + Folder> ExactSizeIterator for FolderOmitDrain<'_, T> {}

// === Primitive folders === //

pub struct RightFolder<'a, T> {
    /// The target [Vec].
    ///
    /// ## Layout
    ///
    /// ```text
    /// [Committed block] [ Dead block ] [Uncommitted block]
    ///                   ^ write index  ^ Read index
    /// ```
    ///
    /// As elements get committed, they get moved from the uncommitted block to the end of the
    /// committed block. Omitted blocks are absorbed into the dead block and immediately dropped. All
    /// elements in the dead block are logically uninitialized and must be removed from the array
    /// before returning control to the user. Thus, even if dropped early, the released `Vec` will
    /// only contain the committed block.
    ///
    target: &'a mut Vec<MaybeUninit<T>>,

    /// The target index that newly committed elements will be written to.
    write_index: usize,

    /// The index from which uncommitted elements will be read from.
    read_index: usize,
}

impl<'a, T> RightFolder<'a, T> {
    pub fn new(target: &'a mut Vec<T>) -> Self {
        Self {
            // FIXME: Is this actually legal?
            target: unsafe { transmute::<&'a mut Vec<T>, &'a mut Vec<MaybeUninit<T>>>(target) },
            write_index: 0,
            read_index: 0,
        }
    }
}

impl<T> Folder for RightFolder<'_, T> {
    type Item = T;

    fn remaining(&self) -> usize {
        self.target.len() - self.read_index
    }

    fn split<'a>(&'a mut self) -> (&'a mut [Self::Item], &'a mut [Self::Item]) {
        // Split vector
        // TODO: Implement n-splitting generically
        let (committed, right) = self.target.split_at_mut(self.write_index);
        let (_, uncommitted) = right.split_at_mut(self.read_index - self.write_index);

        // Commit elements
        (
            unsafe {
                transmute::<&'a mut [MaybeUninit<Self::Item>], &'a mut [Self::Item]>(committed)
            },
            unsafe {
                transmute::<&'a mut [MaybeUninit<Self::Item>], &'a mut [Self::Item]>(uncommitted)
            },
        )
    }

    fn new_len(&self) -> usize {
        self.write_index
    }

    fn commit_many(&mut self, count: usize) {
        debug_assert!(self.read_index + count <= self.target.len());

        for _ in 0..count {
            // Move the item from the uncommitted block to the committed block.
            // The element at the `write_index` was from dead block and will have moved from the
            // start of dead block to the end.
            self.target.swap(self.read_index, self.write_index);
            self.read_index += 1;
            self.write_index += 1;
        }
    }

    fn omit_many(&mut self, count: usize) {
        debug_assert!(self.read_index + count <= self.target.len());

        // N.B. we increment the `read_index` before our calls to drop because users could panic and
        // potentially cause the proceeding block to be filled with logically uninitialized values.
        self.read_index += count;
        for omitted in self.target[self.read_index..].iter_mut().take(count) {
            unsafe { omitted.assume_init_drop() };
        }
    }

    fn take(&mut self) -> Self::Item {
        debug_assert!(self.read_index < self.target.len());

        let taken = unsafe { self.target[self.read_index].assume_init_read() };
        // This slot is now included in the dead block. No need to drop it though because the move
        // already uninitializes it.
        self.read_index += 1;
        taken
    }
}

impl<T> Drop for RightFolder<'_, T> {
    fn drop(&mut self) {
        // Drop elements in the uncommitted block
        for uncommitted in &mut self.target[self.read_index..] {
            unsafe { uncommitted.assume_init_drop() }
        }

        unsafe { self.target.set_len(self.write_index) };
    }
}

#[test]
fn fold_right_test() {
    let mut target = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    let mut folder = RightFolder::new(&mut target);
    assert_eq!(folder.take(), 1);
    folder.commit();
    folder.commit();
    folder.omit_many(2);
    folder.commit_many(2);
    assert_eq!(folder.take_many(2).collect_array(), [8, 9]);
    drop(folder);

    assert_eq!(&*target, &[2, 3, 6, 7]);
}
