// TODO: Code review

use crate::util::reader::{LookaheadReader, StreamReader};
use crate::util::slice_ext::{ArrayCollectExt, TakeAtExt};
use std::mem::{transmute, MaybeUninit};

// === Generic mechanisms === //

pub trait FolderPos: Sized + Copy + Eq + Ord {
    fn as_rel_raw(self, root_loc: usize) -> usize;
    fn as_abs_raw(self, root_loc: usize) -> Abs;

    fn as_rel<F: ?Sized + Folder>(self, folder: &F) -> usize {
        self.as_rel_raw(folder.next_abs_raw())
    }

    fn as_abs<F: ?Sized + Folder>(self, folder: &F) -> Abs {
        self.as_abs_raw(folder.next_abs_raw())
    }
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Abs(pub usize);

impl Abs {
    pub fn add(self, rel: usize) -> Self {
        Self(self.0 + rel)
    }

    pub fn sub(self, rel: usize) -> Self {
        Self(self.0 - rel)
    }

    pub fn next(self) -> Self {
        self.add(1)
    }

    pub fn prev(self) -> Self {
        self.sub(1)
    }

    pub fn is_accessible_by<F: ?Sized + Folder>(self, folder: &F) -> bool {
        self.0 >= folder.next_abs_raw()
    }
}

impl FolderPos for Abs {
    fn as_rel_raw(self, root: usize) -> usize {
        assert!(self.0 >= root);
        self.0 - root
    }

    fn as_abs_raw(self, _root: usize) -> Abs {
        self
    }
}

impl FolderPos for usize {
    fn as_rel_raw(self, _root: usize) -> usize {
        self
    }

    fn as_abs_raw(self, root: usize) -> Abs {
        Abs(root + self)
    }
}

/// Folders a very useful reader-like construct which allow users to procedurally move through an
/// array from either left-to-right ([RightFolder]) or right-to-left ([LeftFolder]) to accept, replace,
/// and skip elements from the array to reduce it.
///
/// This trait is object-safe, with all of the non-object-safe methods being derived in the [FolderExt]
/// extension trait.
pub trait Folder {
    type Item;

    // === List querying === //

    fn next_abs_raw(&self) -> usize;

    fn next_loc(&self) -> Abs {
        (0).as_abs(self)
    }

    fn remaining(&self) -> usize;

    fn free_slots(&self) -> usize;

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

    fn push(&mut self, item: Self::Item);

    // === Simple modification === //

    fn produce(&mut self, count: usize, value: Self::Item) {
        self.reduce(count, value);
        self.commit();
    }

    fn reduce(&mut self, count: usize, value: Self::Item) {
        assert!(self.remaining() >= count);

        self.omit_many(count - 1);
        self.proceeding_mut()[0] = value;
    }
}

pub trait FolderExt: Folder {
    fn reader(&mut self) -> FolderReader<'_, Self::Item> {
        FolderReader {
            remaining: self.proceeding(),
            pos: self.next_loc(),
            prev: None,
        }
    }

    fn lookahead<'a, F, R>(&'a mut self, fn_: F) -> R
    where
        F: FnOnce(&mut FolderReader<'a, Self::Item>) -> R,
    {
        fn_(&mut self.reader())
    }

    fn take_drain(&mut self) -> FolderOmitIter<'_, Self> {
        FolderOmitIter::new(self)
    }

    fn take_many(&mut self, count: usize) -> FolderOmitDrain<'_, Self> {
        FolderOmitDrain::new(self, count)
    }

    fn take_at<T: FolderPos, const N: usize>(&mut self, locs: [T; N]) -> [Self::Item; N] {
        let root = self.next_abs_raw();

        self.take_drain()
            .take_at(locs.iter().map(move |loc| loc.as_rel_raw(root)))
            .collect_array()
    }
}

impl<F: ?Sized + Folder> FolderExt for F {}

pub struct FolderOmitIter<'a, T: ?Sized + Folder> {
    target: &'a mut T,
}

impl<'a, T: ?Sized + Folder> FolderOmitIter<'a, T> {
    pub fn new(target: &'a mut T) -> Self {
        Self { target }
    }
}

impl<T: ?Sized + Folder> Iterator for FolderOmitIter<'_, T> {
    type Item = T::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.target.has_remaining() {
            Some(self.target.take())
        } else {
            None
        }
    }
}

pub struct FolderOmitDrain<'a, T: ?Sized + Folder> {
    target: &'a mut T,
    remaining: usize,
}

impl<'a, T: ?Sized + Folder> FolderOmitDrain<'a, T> {
    pub fn new(target: &'a mut T, count: usize) -> Self {
        assert!(target.remaining() >= count);

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

#[derive(Debug)]
pub struct FolderReader<'a, T> {
    remaining: &'a [T],
    pos: Abs,
    prev: Option<&'a T>,
}

impl<T> Clone for FolderReader<'_, T> {
    fn clone(&self) -> Self {
        Self {
            remaining: self.remaining,
            pos: self.pos,
            prev: self.prev,
        }
    }
}

impl<'a, T> FolderReader<'a, T> {
    pub fn prev(&self) -> Option<&'a T> {
        self.prev
    }

    pub fn remaining(&self) -> &'a [T] {
        self.remaining
    }

    pub fn prev_pos(&self) -> Abs {
        assert!(self.prev.is_some());
        self.pos.prev()
    }

    pub fn next_pos(&self) -> Abs {
        self.pos
    }

    pub fn consume_loc(&mut self) -> Option<(&'a T, Abs)> {
        self.consume().map(|ref_| (ref_, self.prev_pos()))
    }
}

impl<'a, T> StreamReader for FolderReader<'a, T> {
    type Res = Option<&'a T>;

    fn consume(&mut self) -> Self::Res {
        if !self.remaining.is_empty() {
            self.prev = Some(&self.remaining[0]);
            self.pos = self.pos.next();
            self.remaining = &self.remaining[1..];
            self.prev
        } else {
            None
        }
    }
}

impl<T> LookaheadReader for FolderReader<'_, T> {}

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

    fn next_abs_raw(&self) -> usize {
        self.read_index
    }

    fn remaining(&self) -> usize {
        self.target.len() - self.read_index
    }

    fn free_slots(&self) -> usize {
        self.read_index - self.write_index
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
        assert!(self.read_index + count <= self.target.len());

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
        assert!(self.read_index + count <= self.target.len());

        // N.B. we increment the `read_index` before our calls to drop because users could panic and
        // potentially cause the proceeding block to be filled with logically uninitialized values.
        let start = self.read_index;
        self.read_index += count;
        for omitted in &mut self.target[start..self.read_index] {
            unsafe { omitted.assume_init_drop() };
        }
    }

    fn take(&mut self) -> Self::Item {
        assert!(self.read_index < self.target.len());

        let taken = unsafe { self.target[self.read_index].assume_init_read() };
        // This slot is now included in the dead block. No need to drop it though because the move
        // already uninitializes it.
        self.read_index += 1;
        taken
    }

    fn push(&mut self, item: Self::Item) {
        assert!(self.free_slots() > 0);
        self.target[self.write_index] = MaybeUninit::new(item);
        self.write_index += 1;
    }
}

impl<T> Drop for RightFolder<'_, T> {
    fn drop(&mut self) {
        // Semi-questionable lifetime prolongation.
        let drop_slice = self.target.as_mut_slice() as *mut [MaybeUninit<T>];
        let drop_slice = unsafe { &mut (&mut *drop_slice)[self.read_index..] };

        // N.B. we set the length before dropping the elements because users could panic in the middle
        // of the drop loop.
        unsafe { self.target.set_len(self.write_index) };

        // Drop elements in the uncommitted block
        for uncommitted in drop_slice {
            unsafe { uncommitted.assume_init_drop() }
        }
    }
}

#[test]
fn test_equation() {
    // === Ast === //
    #[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
    enum BinOp {
        Add,
        Mul,
        Sub,
        Div,
    }

    #[derive(Debug, Clone)]
    enum Atom {
        Expr(Expr),
        Op(BinOp),
    }

    #[derive(Debug, Clone)]
    enum Expr {
        Lit(i32),
        Bin {
            lhs: Box<Expr>,
            rhs: Box<Expr>,
            op: BinOp,
        },
    }

    // === Example logic === //
    fn lit(a: i32) -> Atom {
        Atom::Expr(Expr::Lit(a))
    }

    let mut target = vec![
        lit(2),
        Atom::Op(BinOp::Add),
        lit(4),
        Atom::Op(BinOp::Mul),
        lit(5),
    ];

    fn fold_bin_op<F: Folder<Item = Atom>>(folder: &mut F, ops: &[BinOp]) {
        while folder.has_remaining() {
            if let Some((lhs, op, rhs)) = folder.lookahead(|reader| {
                let lhs = match reader.consume_loc() {
                    Some((Atom::Expr(_), pos)) => pos,
                    _ => return None,
                };

                let op = match reader.consume_loc() {
                    Some((Atom::Op(op), _)) if ops.contains(op) => op,
                    _ => return None,
                };

                let rhs = match reader.consume_loc() {
                    Some((Atom::Expr(_), pos)) => pos,
                    _ => return None,
                };

                Some((lhs, *op, rhs))
            }) {
                let [lhs, rhs] = folder.take_at([lhs, rhs]);
                let lhs = match lhs {
                    Atom::Expr(expr) => expr,
                    _ => unreachable!(),
                };

                let rhs = match rhs {
                    Atom::Expr(expr) => expr,
                    _ => unreachable!(),
                };

                folder.push(Atom::Expr(Expr::Bin {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op,
                }));

                continue;
            }

            folder.commit();
        }
    }

    fold_bin_op(
        &mut RightFolder::new(&mut target),
        &[BinOp::Mul, BinOp::Div],
    );

    fold_bin_op(
        &mut RightFolder::new(&mut target),
        &[BinOp::Add, BinOp::Sub],
    );

    assert_eq!(target.len(), 1);
    println!("Folded result:\n{:#?}", target);
}
