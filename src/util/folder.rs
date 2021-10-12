// TODO: Code review

use crate::util::slice_ext::ArrayCollectExt;
use std::mem::{transmute, MaybeUninit};

// === Generic mechanisms === //

// #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
// pub struct FolderLoc {
//     pub index: usize,
// }
//
// impl FolderLoc {
//     pub fn add(self, rel: usize) -> Self {
//         Self {
//             index: self.index + rel,
//         }
//     }
//
//     pub fn sub(self, rel: usize) -> Self {
//         Self {
//             index: self.index - rel,
//         }
//     }
//
//     pub fn next(self) -> Self {
//         self.add(1)
//     }
//
//     pub fn prev(self) -> Self {
//         self.sub(1)
//     }
// }

/// Folders a very useful reader-like construct which allow users to procedurally move through an
/// array from either left-to-right ([RightFolder]) or right-to-left ([LeftFolder]) to accept, replace,
/// and skip elements from the array to reduce it.
///
/// This trait is object-safe, with all of the non-object-safe methods being derived in the [FolderExt]
/// extension trait.
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

    fn preceding(&self) -> &[Self::Item];

    fn proceeding(&self) -> &[Self::Item];

    fn split_mut(&mut self) -> (&mut [Self::Item], &mut [Self::Item]);

    fn preceding_mut(&mut self) -> &mut [Self::Item] {
        self.split_mut().0
    }

    fn proceeding_mut(&mut self) -> &mut [Self::Item] {
        self.split_mut().1
    }

    // fn get_rel(&self, rel: usize) -> &Self::Item {
    //     &self.proceeding()[rel]
    // }
    //
    // fn get_rel_mut(&mut self, rel: usize) -> &mut Self::Item {
    //     &mut self.proceeding_mut()[rel]
    // }
    //
    // fn get_at(&self, loc: FolderLoc) -> &Self::Item {
    //     self.get_rel(self.resolve_rel_index(loc))
    // }
    //
    // fn get_at_mut(&mut self, loc: FolderLoc) -> &mut Self::Item {
    //     self.get_rel_mut(self.resolve_rel_index(loc))
    // }

    // === Primitive list modification === //

    fn new_len(&self) -> usize;

    fn commit_many(&mut self, count: usize);

    fn commit(&mut self) {
        self.commit_many(1);
    }

    // fn commit_until(&mut self, loc: FolderLoc) {
    //     self.commit_many(self.resolve_rel_index(loc))
    // }

    fn omit_many(&mut self, count: usize);

    fn omit(&mut self) {
        self.omit_many(1);
    }

    // fn omit_until(&mut self, loc: FolderLoc) {
    //     self.omit_many(self.resolve_rel_index(loc))
    // }

    fn take(&mut self) -> Self::Item;

    // === Simple modification === //

    fn produce(&mut self, count: usize, value: Self::Item) {
        self.reduce(count, value);
        self.commit();
    }

    // fn produce_until(&mut self, loc: FolderLoc, value: Self::Item) {
    //     self.produce(self.resolve_rel_index(loc), value)
    // }

    fn reduce(&mut self, count: usize, value: Self::Item) {
        debug_assert!(self.remaining() >= count);

        self.proceeding_mut()[count - 1] = value;
        self.omit_many(count - 1);
    }

    // fn reduce_until(&mut self, loc: FolderLoc, value: Self::Item) {
    //     self.reduce(self.resolve_rel_index(loc), value)
    // }

    // === Index system === //

    // fn root_loc(&self) -> usize;
    //
    // fn can_touch(&self, loc: FolderLoc) -> bool {
    //     loc.index >= self.root_loc()
    // }
    //
    // fn resolve_rel_index(&self, loc: FolderLoc) -> usize {
    //     assert!(self.can_touch(loc));
    //     loc.index - self.root_loc()
    // }
    //
    // fn loc_of(&self, rel: usize) -> FolderLoc {
    //     FolderLoc {
    //         index: self.root_loc() + rel,
    //     }
    // }
    //
    // fn loc_of_first(&self) -> FolderLoc {
    //     self.loc_of(0)
    // }
    //
    // fn swap(&mut self, a: FolderLoc, b: FolderLoc) {
    //     let (a_idx, b_idx) = (self.resolve_rel_index(a), self.resolve_rel_index(b));
    //     self.proceeding_mut().swap(a_idx, b_idx);
    // }
    //
    // fn swap_tracked(&mut self, a: &mut FolderLoc, b: &mut FolderLoc) {
    //     self.swap(*a, *b);
    //     std::mem::swap(a, b);
    // }
}

pub trait FolderExt: Folder {
    fn take_many(&mut self, count: usize) -> FolderOmitDrain<'_, Self> {
        FolderOmitDrain::new(self, count)
    }

    // fn reader(&self) -> FolderReader<'_, Self::Item> {
    //     FolderReader::new(self)
    // }
    //
    // fn peek<F, R>(&self, handler: F) -> R
    // where
    //     F: FnOnce(&mut FolderReader<'_, Self::Item>) -> R,
    // {
    //     handler(&mut self.reader())
    // }
}

impl<F: ?Sized + Folder> FolderExt for F {}

// pub struct FolderReader<'a, T> {
//     iter: AnnotatedFolderIter<'a, T>,
// }
//
// impl<T> Clone for FolderReader<'_, T> {
//     fn clone(&self) -> Self {
//         Self {
//             iter: self.iter.clone(),
//         }
//     }
// }
//
// impl<T> LookaheadReader for FolderReader<'_, T> {}
//
// impl<'a, T> FolderReader<'a, T> {
//     pub fn new<F: ?Sized + Folder<Item = T>>(folder: &'a F) -> Self {
//         Self {
//             iter: AnnotatedFolderIter {
//                 slice: folder.proceeding(),
//                 loc: folder.loc_of_first(),
//             },
//         }
//     }
//
//     pub fn remaining(&self) -> &'a [T] {
//         self.iter.slice
//     }
//
//     pub fn remaining_annotated(&self) -> AnnotatedFolderIter<'a, T> {
//         self.iter.clone()
//     }
//
//     pub fn consume_loc(&mut self) -> Option<(FolderLoc, &'a T)> {
//         self.iter.next()
//     }
//
//     pub fn consume(&mut self) -> Option<&'a T> {
//         self.consume_loc().map(|(_, ref_)| ref_)
//     }
//
//     pub fn peek_loc(&self) -> Option<(FolderLoc, &'a T)> {
//         self.clone().consume_loc()
//     }
//
//     pub fn peek(&self) -> Option<&'a T> {
//         self.clone().consume()
//     }
//
//     pub fn next_loc(&self) -> FolderLoc {
//         self.iter.loc
//     }
//
//     pub fn prev_loc(&self) -> FolderLoc {
//         self.iter.loc.prev()
//     }
// }
//
// pub struct AnnotatedFolderIter<'a, T> {
//     slice: &'a [T],
//     loc: FolderLoc,
// }
//
// impl<T> Clone for AnnotatedFolderIter<'_, T> {
//     fn clone(&self) -> Self {
//         Self {
//             slice: self.slice,
//             loc: self.loc,
//         }
//     }
// }
//
// impl<'a, T> Iterator for AnnotatedFolderIter<'a, T> {
//     type Item = (FolderLoc, &'a T);
//
//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(first) = self.slice.first() {
//             let loc = self.loc.next();
//             self.slice = &self.slice[1..];
//             self.loc = self.loc.next();
//             Some((loc, first))
//         } else {
//             None
//         }
//     }
//
//     fn size_hint(&self) -> (usize, Option<usize>) {
//         (self.slice.len(), Some(self.slice.len()))
//     }
//
//     fn count(self) -> usize
//     where
//         Self: Sized,
//     {
//         self.slice.len()
//     }
// }
//
// impl<'a, T> ExactSizeIterator for AnnotatedFolderIter<'a, T> {}

pub struct FolderOmitDrain<'a, T: ?Sized + Folder> {
    target: &'a mut T,
    remaining: usize,
}

impl<'a, T: ?Sized + Folder> FolderOmitDrain<'a, T> {
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

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.remaining
    }
}

impl<T: ?Sized + Folder> ExactSizeIterator for FolderOmitDrain<'_, T> {}

impl<T: ?Sized + Folder> Drop for FolderOmitDrain<'_, T> {
    fn drop(&mut self) {
        self.target.omit_many(self.remaining);
    }
}

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

    fn preceding<'a>(&'a self) -> &'a [Self::Item] {
        unsafe {
            transmute::<&'a [MaybeUninit<Self::Item>], &'a [Self::Item]>(
                &self.target[..self.write_index],
            )
        }
    }

    fn proceeding<'a>(&'a self) -> &'a [Self::Item] {
        unsafe {
            transmute::<&'a [MaybeUninit<Self::Item>], &'a [Self::Item]>(
                &self.target[self.read_index..],
            )
        }
    }

    fn split_mut<'a>(&'a mut self) -> (&'a mut [Self::Item], &'a mut [Self::Item]) {
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

    // fn root_loc(&self) -> usize {
    //     self.read_index
    // }
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
fn fold_right_primitive_test() {
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
