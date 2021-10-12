pub trait LookaheadReader: Clone {
    /// Attempts to match the reader sequence using the handler, committing the state if the return
    /// value is truthy (*e.g.* `true`, `Some(_)`, `Ok(_)`; see [LookaheadResult] for details) and
    /// ignoring all reader state changes otherwise.
    fn lookahead<F, R>(&mut self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        let mut lookahead = self.clone();
        let res = handler(&mut lookahead);
        if res.is_truthy() {
            *self = lookahead;
        }
        res.into_result()
    }

    fn peek_ahead<F, R>(&self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.clone()).into_result()
    }

    fn consume_while<F>(&mut self, mut handler: F)
    where
        F: FnMut(&mut Self) -> bool,
    {
        while self.lookahead(&mut handler) {}
    }
}

pub trait LookaheadResult {
    type Ret;

    fn is_truthy(&self) -> bool;
    fn into_result(self) -> Self::Ret;
}

impl LookaheadResult for bool {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        *self
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for Option<T> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_some()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_ok()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for (bool, T) {
    type Ret = T;

    fn is_truthy(&self) -> bool {
        self.0
    }

    fn into_result(self) -> Self::Ret {
        self.1
    }
}

/*
/// A stream reader capable of backtracking to one of its previous states, allowing it to look ahead
/// while matching.
///
/// ## Forking Internals
///
/// Representing forking mechanisms accurately is a surprisingly difficult challenge. We have a
/// variety of readers, each with their own requirements. There are two major kinds of readers:
///
/// - *Immutably borrowed readers*, which borrow all their targets immutably, making their borrow
///   durations independent of the duration of the mutable [fork] borrow.
/// - *Mutably borrowed readers*, which borrow at least one of their targets mutably, making their borrow
///   durations dependent of the duration of the mutable [fork] borrow.
///
/// Our solution is to have users create a fork of the reader which the users will actually modify
/// and either drop the fork if the lookahead pattern cannot be matched or commit it with [commit_fork]
/// if it is. The specific type of the fork is determined by the user's [AsFork] type alias, which
/// is parameterized by the lifetime of the mutable borrow required to create the fork.
///
/// To apply the fork, we have to somehow provably end the fork's mutable borrow of the parent while
/// persisting its state so that it can be applied. To achieve this, we apply forks in two stages:
/// first, we decompose the fork into its non-lifetime-limited components through [decompose_fork] and
/// then we call the [commit_fork] method which does the actual mutable borrowing of the parent to
/// apply the decomposed state.
///
/// In doing so, both immutably and mutably borrowed readers can be handled by the same trait.
pub trait LookaheadReader {
    // === Core forking === //

    type Decomposed;
    #[rustfmt::skip]
    type AsFork<'a> where Self: 'a;

    /// Constructs a fork of this reader which can later be committed back into this reader's state or
    /// dropped if the fork was unable to match anything useful.
    fn fork(&mut self) -> Self::AsFork<'_>;

    /// Decomposes a fork which may potentially be borrowing this reader into a version of its state
    /// that is no longer borrowing the reader.
    fn decompose_fork<'a>(fork: Self::AsFork<'a>) -> Self::Decomposed
    where
        Self: 'a;

    /// Applies the decomposed version of a fork produced by [decompose_fork] to the current reader.
    fn commit_fork(&mut self, state: Self::Decomposed);

    // === Lookahead === //

    /// Attempts to match the reader sequence using the handler, committing the state if the return
    /// value is truthy (*e.g.* `true`, `Some(_)`, `Ok(_)`; see [LookaheadResult] for details) and
    /// ignoring all reader state changes otherwise.
    fn lookahead<F, R>(&mut self, handler: F) -> R::Ret
    where
        // FIXME: Because `'_` can have any lifetime, the `Self: '_` clause causes `Self` to be inferred as living for `'static`.
        F: FnOnce(&mut Self::AsFork<'_>) -> R,
        R: LookaheadResult,
    {
        let mut fork = self.fork();
        let res = handler(&mut fork);
        if res.is_truthy() {
            let fork = Self::decompose_fork(fork);
            self.commit_fork(fork);
        }
        res.into_result()
    }

    fn peek_ahead<F, R>(&mut self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self::AsFork<'_>) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.fork()).into_result()
    }

    fn consume_while<F>(&mut self, mut handler: F)
    where
        F: FnMut(&mut Self::AsFork<'_>) -> bool,
    {
        while self.lookahead(&mut handler) {}
    }
}

pub trait LookaheadResult {
    type Ret;

    fn is_truthy(&self) -> bool;
    fn into_result(self) -> Self::Ret;
}

impl LookaheadResult for bool {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        *self
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for Option<T> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_some()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        self.is_ok()
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

impl<T> LookaheadResult for (bool, T) {
    type Ret = T;

    fn is_truthy(&self) -> bool {
        self.0
    }

    fn into_result(self) -> Self::Ret {
        self.1
    }
}

// === Utility readers === //

pub struct SliceMutReader<'a, T> {
    /// The target slice being read from.
    target: &'a mut [T],

    /// An offset from the start of slice. This offset must never decrease.
    base: usize,
}

#[doc(hidden)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct SliceReaderMutState {
    slice_addr: NonNull<()>,
    base: usize,
}

impl<'a, T> SliceMutReader<'a, T> {
    pub fn new(target: &'a mut [T]) -> Self {
        Self { target, base: 0 }
    }

    pub fn consume_many(&mut self, count: usize) -> Option<&'a mut [T]> {
        if self.base + count <= self.target.len() {
            let elem: &'a mut [T] = unsafe {
                // Safety: we know that we borrow the slice mutably for `'a` and that this object will
                // have exclusive ownership over the slice for that long. Because this method consumes
                // the element, neither this reader nor its parents will go over it again, making this
                // lifetime prolongation valid.
                let base = self.target.as_mut_ptr().add(self.base);
                &mut *slice_from_raw_parts_mut(base, count)
            };
            self.base += count;
            Some(elem)
        } else {
            None
        }
    }

    pub fn consume(&mut self) -> Option<&'a mut T> {
        self.consume_many(1).map(|slice| slice.first_mut().unwrap())
    }

    pub fn peek(&self) -> Option<&T> {
        self.as_slice().first()
    }

    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.as_slice_mut().first_mut()
    }

    pub fn as_slice(&self) -> &[T] {
        &self.target[self.base..]
    }

    pub fn as_slice_mut(&mut self) -> &mut [T] {
        &mut self.target[self.base..]
    }

    pub fn remaining(&self) -> usize {
        self.target.len() - self.base
    }

    pub fn has_remaining(&self) -> bool {
        self.remaining() > 0
    }

    pub fn is_fully_processed(&self) -> bool {
        self.remaining() == 0
    }

    pub fn consumed(&self) -> usize {
        self.base
    }
}

impl<T> LookaheadReader for SliceMutReader<'_, T> {
    type Decomposed = SliceReaderMutState;
    #[rustfmt::skip]
    type AsFork<'a> where Self: 'a = SliceMutReader<'a, T>;

    fn fork(&mut self) -> Self::AsFork<'_> {
        SliceMutReader {
            target: self.target,
            base: self.base,
        }
    }

    fn decompose_fork<'a>(fork: Self::AsFork<'a>) -> Self::Decomposed
    where
        Self: 'a,
    {
        SliceReaderMutState {
            slice_addr: NonNull::from(fork.target).cast(),
            base: fork.base,
        }
    }

    fn commit_fork(&mut self, state: Self::Decomposed) {
        assert_eq!(NonNull::from(&*self.target).cast(), state.slice_addr);
        self.base = state.base;
    }
}
*/
