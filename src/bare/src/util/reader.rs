use crate::util::iter_ext::{ControlFlowIter, IteratorAdapterSingle, RepFlow, ToIterControlFlow};
use std::cmp::Ordering;
use std::marker::PhantomData;

// === Lookahead machinery === //

/// Readers are cursors which allow users to procedurally match a grammar. `LookaheadReaders` are
/// special because they allow users to fork the cursor and "look-ahead" an arbitrary number of
/// elements to see if the grammar matches.
pub trait LookaheadReader: Sized + Clone {
    /// Attempts to match the reader sequence using the handler, committing the state if the return
    /// value is truthy (*e.g.* `true`, `Some(_)`, `Ok(_)`; see [LookaheadResult] for details) and
    /// ignoring all reader state changes otherwise.
    fn lookahead<F, R>(&mut self, handler: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        let mut lookahead = self.clone();
        let res = handler(&mut lookahead);
        if res.should_commit() {
            *self = lookahead;
        }
        res
    }

    /// Forks the reader and peaks ahead without ever committing the result.
    fn peek_ahead<F, R>(&self, handler: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.clone())
    }

    /// Consumes from the reader while the grammar matches and returns an iterator of the elements
    /// returned by each iteration. Reader state from unmatched iterations are discarded as if they
    /// were passed through [lookahead].
    fn consume_while<F, R>(&mut self, handler: F) -> ConsumeWhileIter<Self, F, R>
    where
        F: FnMut(&mut Self) -> R,
        R: RepeatResult,
    {
        ConsumeWhileIter::new(self, handler)
    }

    fn branch<T>(&mut self) -> ChoiceMatcher<Self, T>
    where
        T: LookaheadResult + ErrorAccumulator,
    {
        ChoiceMatcher::new(self)
    }
}

/// A [LookaheadResult] tells [LookaheadReader::lookahead] whether the lookahead matched.
pub trait LookaheadResult {
    fn should_commit(&self) -> bool;
}

impl LookaheadResult for bool {
    fn should_commit(&self) -> bool {
        *self
    }
}

impl<T> LookaheadResult for Option<T> {
    fn should_commit(&self) -> bool {
        self.is_some()
    }
}

impl<T, E> LookaheadResult for Result<T, E> {
    fn should_commit(&self) -> bool {
        self.is_ok()
    }
}

impl<T> LookaheadResult for RepFlow<T> {
    fn should_commit(&self) -> bool {
        match self {
            RepFlow::None => false,
            _ => true,
        }
    }
}

// === Repetition matching === //

pub trait RepeatResult: Sized + LookaheadResult + ToIterControlFlow {}

impl<T: Sized + LookaheadResult + ToIterControlFlow> RepeatResult for T {}

pub struct ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: RepeatResult,
{
    _ty: PhantomData<fn() -> O>,
    handler: F,
    reader: &'a mut R,
    iter: ControlFlowIter,
}

impl<'a, R, F, O> ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: RepeatResult,
{
    pub fn new(reader: &'a mut R, handler: F) -> Self {
        Self {
            _ty: PhantomData,
            handler,
            reader,
            iter: ControlFlowIter::new(),
        }
    }
}

impl<'a, R, F, O> Iterator for ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: RepeatResult,
{
    type Item = <O as ToIterControlFlow>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let res = self.reader.lookahead(&mut self.handler);
        self.iter.next_single(res.to_control_flow_option())
    }
}

impl<'a, R, F, O> Drop for ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: RepeatResult,
{
    fn drop(&mut self) {
        for _ in self {}
    }
}

// === Delimited list matching === //

pub struct DelimiterMatcher<Reader, FDel, Res> {
    _sig: PhantomData<fn(&mut Reader) -> Res>,
    match_del: FDel,
    first: bool,
}

impl<Reader, FDel, Res> Clone for DelimiterMatcher<Reader, FDel, Res>
where
    FDel: Clone,
{
    fn clone(&self) -> Self {
        Self {
            _sig: PhantomData,
            match_del: self.match_del.clone(),
            first: self.first,
        }
    }
}

impl<Reader, FDel, Res> DelimiterMatcher<Reader, FDel, Res>
where
    Reader: LookaheadReader,
    FDel: FnMut(&mut Reader) -> Res,
    Res: LookaheadResult + Default,
{
    pub fn new(match_del: FDel, require_leading: bool) -> Self {
        Self {
            _sig: PhantomData,
            match_del,
            first: !require_leading,
        }
    }

    pub fn new_start(match_del: FDel) -> Self {
        Self::new(match_del, false)
    }

    pub fn is_first(&self) -> bool {
        self.first
    }

    pub fn next(&mut self, reader: &mut Reader) -> Option<Res> {
        if self.first {
            self.first = false;
            Some(Default::default())
        } else {
            let res = reader.lookahead(&mut self.match_del);
            if res.should_commit() {
                Some(res)
            } else {
                None
            }
        }
    }
}

// === Branch matching === //

/// A [LookaheadResult] with a clear error variant and a way to accumulate uncommitted values into a
/// single error.
pub trait ErrorAccumulator {
    fn empty_error() -> Self;
    fn extend_error(&mut self, error: Self);
}

impl ErrorAccumulator for bool {
    fn empty_error() -> Self {
        false
    }

    fn extend_error(&mut self, _: Self) {}
}

impl<T> ErrorAccumulator for Option<T> {
    fn empty_error() -> Self {
        None
    }

    fn extend_error(&mut self, _: Self) {}
}

impl<T, E: ErrorAccumulator> ErrorAccumulator for Result<T, E> {
    fn empty_error() -> Self {
        Err(E::empty_error())
    }

    fn extend_error(&mut self, error: Self) {
        self.as_mut()
            .err()
            .unwrap()
            .extend_error(error.err().unwrap());
    }
}

#[derive(Debug)]
#[must_use]
pub struct ChoiceMatcher<'a, R, T> {
    reader: &'a mut R,
    state: ChoiceMatcherState,
    result: T,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum ChoiceMatcherState {
    Passed,
    Barred,
    Failing,
}

impl<'a, R, T> ChoiceMatcher<'a, R, T>
where
    R: LookaheadReader,
    T: ErrorAccumulator + LookaheadResult,
{
    pub fn new(reader: &'a mut R) -> Self {
        Self {
            reader,
            state: ChoiceMatcherState::Failing,
            result: T::empty_error(),
        }
    }

    pub fn case<F>(mut self, handler: F) -> Self
    where
        F: FnOnce(&mut R) -> T,
    {
        if self.state == ChoiceMatcherState::Barred {
            return self;
        }

        let result = self.reader.lookahead(handler);
        if result.should_commit() {
            debug_assert_eq!(self.state, ChoiceMatcherState::Failing);
            self.result = result;
            self.state = ChoiceMatcherState::Passed;
        } else {
            self.result.extend_error(result);
        }

        self
    }

    pub fn barrier(mut self) -> Self {
        if self.state == ChoiceMatcherState::Passed {
            self.state = ChoiceMatcherState::Barred;
        }
        self
    }

    pub fn done(self) -> T {
        self.result
    }
}

// === Generic stream machinery === //

pub trait StreamReader: Sized {
    type Res: ToIterControlFlow;

    fn consume(&mut self) -> Self::Res;

    fn as_drain(&mut self) -> StreamDrain<'_, Self> {
        StreamDrain::new(self)
    }

    fn peek(&self) -> Self::Res
    where
        Self: LookaheadReader,
    {
        self.clone().consume()
    }
}

#[derive(Debug)]
pub struct StreamDrain<'a, R> {
    pub reader: &'a mut R,
    state: ControlFlowIter,
}

impl<'a, R> StreamDrain<'a, R> {
    pub fn new(reader: &'a mut R) -> Self {
        Self {
            reader,
            state: Default::default(),
        }
    }
}

impl<'a, R: ?Sized + StreamReader> Iterator for StreamDrain<'a, R> {
    type Item = <R::Res as ToIterControlFlow>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.state
            .next_single(self.reader.consume().to_control_flow_option())
    }
}

// === Location tracking === //

pub trait FileLoc: Ord {}

pub trait LocatedReader {
    type Loc: FileLoc;

    fn prev_loc(&self) -> Self::Loc;
    fn next_loc(&self) -> Self::Loc;
}

// === Unstuck tracking === //

#[derive(Debug, Clone)]
pub struct UnstuckReporter<L, E> {
    expectations: Option<(L, Vec<E>)>,
}

impl<L, E> Default for UnstuckReporter<L, E> {
    fn default() -> Self {
        Self { expectations: None }
    }
}

impl<L, E> UnstuckReporter<L, E> {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<L: FileLoc, E> UnstuckReporter<L, E> {
    pub fn push_expectation(&mut self, loc: L, expectation: E) {
        let (latest_loc, expectations) = match &mut self.expectations {
            Some(expectation) => expectation,
            None => {
                self.expectations = Some((loc, vec![expectation]));
                return;
            }
        };

        match (&*latest_loc).cmp(&loc) {
            Ordering::Less => {
                *latest_loc = loc;
                expectations.clear();
                expectations.push(expectation);
            }
            Ordering::Equal => {
                expectations.push(expectation);
            }
            Ordering::Greater => {
                // (no op)
            }
        }
    }

    pub fn pad_expectation(&mut self, loc: L) {
        let (latest_loc, expectations) = match &mut self.expectations {
            Some(expectation) => expectation,
            None => {
                self.expectations = Some((loc, Vec::new()));
                return;
            }
        };

        if &*latest_loc < &loc {
            *latest_loc = loc;
            expectations.clear();
        }
    }

    pub fn expectations(&self) -> (Option<&L>, &[E]) {
        match &self.expectations {
            Some((loc, expectations)) => (Some(loc), expectations.as_slice()),
            None => (None, &[]),
        }
    }
}

pub trait UnstuckReader: LocatedReader {
    type Error;

    fn reporter(&self) -> &UnstuckReporter<Self::Loc, Self::Error>;
    fn reporter_mut(&mut self) -> &mut UnstuckReporter<Self::Loc, Self::Error>;

    fn expecting(&mut self, expectation: Self::Error) {
        let loc = self.next_loc();
        self.expecting_at(loc, expectation);
    }

    fn expecting_at(&mut self, loc: Self::Loc, expectation: Self::Error) {
        self.reporter_mut().push_expectation(loc, expectation);
    }

    fn pad_expecting(&mut self) {
        let loc = self.next_loc();
        self.pad_expecting_at(loc);
    }

    fn pad_expecting_at(&mut self, loc: Self::Loc) {
        self.reporter_mut().pad_expectation(loc);
    }
}
