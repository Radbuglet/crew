// === Generic === //

/// Folders a very useful reader-like construct which allow users to procedurally move through an
/// array from either left-to-right ([RightFolder]) or right-to-left ([LeftFolder]) to accept, replace,
/// and skip elements from the array to shorten it. They can be used in conjunction with [FolderReaders]
/// to match sequences of elements and merge them into a single element, a practice which is common
/// in expression list to expression tree conversion.
pub trait Folder {
    type Item;

    // === List querying === //

    fn remaining(&self) -> usize;

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

    // === Higher-order list modification === //

    fn merge_start(&mut self, count: usize, value: Self::Item) {
        debug_assert!(self.remaining() >= count);

        self.proceeding()[0] = value;
        self.commit();
        self.omit_many(count - 1);
    }

    fn merge_end(&mut self, count: usize, value: Self::Item) {
        debug_assert!(self.remaining() >= count);

        self.proceeding()[count - 1] = value;
        self.omit_many(count - 1);
        self.commit();
    }
}

// TODO: Implement FolderReader (just a simple iter reader with merge methods)

// === Primitive folders === //

pub struct RightFolder<'a, T> {
    /// The target [Vec].
    ///
    /// ## Layout
    ///
    /// ```text
    /// [Committed block] [Optional space] [Uncommitted block]
    ///                 ^ write index      ^ Read index
    /// ```
    ///
    /// As elements get committed, they get moved from the uncommitted block to the committed block.
    /// All elements in the uncommitted block to the left of the `read_index` are liable to be
    /// overwritten by elements committed to the committed block.
    ///
    target: &'a mut Vec<T>,

    /// The target index that newly committed elements will be written to.
    write_index: usize,

    /// The index from which uncommitted elements will be read from.
    read_index: usize,
}

impl<'a, T> RightFolder<'a, T> {
    pub fn new(target: &'a mut Vec<T>) -> Self {
        Self {
            target,
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

    fn split(&mut self) -> (&mut [Self::Item], &mut [Self::Item]) {
        // TODO: Implement n-splitting generically
        let (committed, right) = self.target.split_at_mut(self.write_index);
        let (_, uncommitted) = right.split_at_mut(self.read_index - self.write_index);
        (committed, uncommitted)
    }

    fn new_len(&self) -> usize {
        self.write_index
    }

    fn commit_many(&mut self, count: usize) {
        debug_assert!(self.read_index + count < self.target.len());

        for _ in 0..count {
            self.target.swap(self.read_index, self.write_index);
            self.read_index += 1;
            self.write_index += 1;
        }
    }

    fn omit_many(&mut self, count: usize) {
        debug_assert!(self.read_index + count < self.target.len());
        self.read_index += count;
    }
}

impl<T> Drop for RightFolder<'_, T> {
    fn drop(&mut self) {
        self.target.truncate(self.write_index);
    }
}
