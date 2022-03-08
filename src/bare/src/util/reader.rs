use std::marker::PhantomData;

// === Lookahead machinery === //

/// Readers are cursors which allow users to procedurally match a grammar. [LookaheadReaders] are
/// special because they allow users to fork the cursor and "look-ahead" an arbitrary number of
/// elements to see if the grammar matches.
///
/// **Match functions** are functions which take in a `LookaheadReader` (also known as a "cursor") and
/// produce an abstract-syntax-tree representation of that grammar, mapping fairly well to the notion
/// of [production rules]. However, match functions have one important additional responsibility. In
/// theory, while the entire grammar of a language could be built up of silently matching production
/// rules with the validity of the root-most rule determining whether the program is valid, the user
/// would loose context as to why their syntax was invalid. Thus, match functions have the additional
/// responsibility of yielding appropriate diagnostic messages on a grammar mismatch, and recovering
/// to a safer state to continue parsing.
///
/// ## Extension Callbacks
///
/// This additional responsibility complicates matters when there is overlap between a match function's
/// grammatical requirements and the grammatical capabilities of subsequent match functions. Consider,
/// for example, the following syntax:
///
/// ```no_run
/// use foo::bar::baz::{elem_1, elem_2};
///
/// fn my_func() -> u32 {
///      maz::laz::SOME_STATIC + 5
/// }
/// ```
///
/// There are two contexts in which a path (the `foo::bar::baz` syntax alone) can be used: a `use`
/// item where each path can be ended with a `"::" + <terminator>` and in expressions where this is
/// not possible. It's tempting to construct a match function that just matches this first "simple path"
/// part and then compose that with another function to match the optional terminator, however doing
/// so complicates diagnostic handling.
///
/// In a non-composed implementation of a path tree parser, we could say that turbo (`::`) delimiters
/// must be followed by either a non-keyword identifier or a braced group of nested paths. In the
/// composed context where we concatenate simple `$(part:ident)::*` matching behavior with the optional
/// `$(:: $terminator:PathTree)?` syntax extension, we introduce a grammatical overlap as the simple
/// parser must now ignore incomplete turbos to allow subsequent matchers to handle them. While this
/// shouldn't have enabled any new illegal grammar (otherwise, the grammar would have been ambiguous),
/// the diagnostics become much less contextual:
///
/// ```no_run
///  use foo::bar::baz::;
///  //               ^ the error is technically the **simple path** not being followed by a `:: + {<path list>}` or a `;`.
///  //                 ^ however, the actual error is that **turbos** can either be followed by a `;` or a `{<path list>}`.
///
/// static val foo: bar::baz:: = ...;
/// //                      ^ the error is technically the simple path not being followed by a `=` or `;`
/// //                         ^ however, the error is intuitively the lack of an identifier after the turbo.
/// ```
///
/// To fix this, matcher functions can take matcher closures to extend their grammar. In this case,
/// the `match_simple_path` can accept a closure taking a mutable [LookaheadReader] reference, which
/// can provide an additional way to match an unclosed terminator. This both enables the primary
/// function to detect illegal terminators (since the closure could produce the appropriate error in
/// the correct context) and would allow the grammar extensions to work off context from the parent
/// function which would have otherwise been discarded.
///
/// Extension callbacks are preferred, even when users could pass along the terminal state as a
/// return value, because this alternative method introduces additional complexity:
///
/// 1. The solution works worse with [LookaheadReader::lookahead]. Regular match methods either
///    commit the successful lookahead state or reject it. However, this alternative approach to the
///    "simple path" parser leaves some ambiguity up in the air. Should it commit the trailing turbo
///    or not? If it doesn't, it could force users to redo work. If it did, how could users return to
///    a state before the turbo was matched?
/// 2. The solution works worse with [match_choice] error building. [match_choice] can automatically
///    combine parse errors into one giant "unexpected <element>, expected <list of choices>" error
///    message. Without this additional mechanism, users would have to extend this error message
///    manually. In other words, while it may *feel* like a match closure is a terminal call, it
///    oftentimes isn't; constructs like [match_choice] have to do additional processing of the
///    returned result.
/// 3. Closure adapters like `with_impossible` and `impossible` don't work. Instead, users wanting to
///    parse a regular simple path without any extensions must manually handle the flag, which is more
///    work.
/// 4. It breaks convention.
///
/// Rather than try to make an abstract decision between composition and extension callbacks, users
/// should compose *complete* matcher functions but fall back to extension callbacks where doing so
/// is not possible.
///
/// ## Cursor Recovery
///
/// Because of the heavyweight nature of compilers, it is better for the users if parsers can recover
/// from certain classes of syntax errors and continue parsing so that the compiler can emit and users
/// can address as many diagnostics as possible at once.
///
/// When a match function detects a potential syntax error that a) it knows cannot be matched by other
/// functions and b) can reasonably recover from, it is encouraged to skip the offending grammar to
/// put the cursor back into a valid state, produce an AST fragment such that the lookahead accepts
/// the result as a "successful parse", and push its errors to a passive diagnostic reporter.
///
/// TODO: Add a way to limit lookahead using a wrapper (e.g. a lookahead_limited method)?
///
/// [LookaheadReaders]: LookaheadReader
/// [production rules]: https://en.wikipedia.org/wiki/Production_(computer_science)
/// [lookahead]: LookaheadReader::lookahead
pub trait LookaheadReader: Clone {
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
        R: LookaheadResult + RepeatResult,
    {
        ConsumeWhileIter::new(self, handler)
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

// === Repetition matching === //

/// A [RepeatResult] tells a [ConsumeWhileIter] whether it should continue iterating, return a final
/// value, or finish without returning anything. This happens entirely independently from
/// [LookaheadResult], which tells the iterator whether to keep result of an iteration.
pub trait RepeatResult: Sized + LookaheadResult {
    type RepeatRes;

    /// Attempts to unwrap the result into the value returned by the iterator. The first element
    /// of the tuple indicates whether to attempt to continue iterating. The iterator will stop
    /// without emitting anything if the result is `None`.
    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)>;
}

impl RepeatResult for bool {
    type RepeatRes = ();

    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)> {
        match self {
            true => Some((true, ())),
            false => None,
        }
    }
}

impl<T> RepeatResult for Option<T> {
    type RepeatRes = T;

    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)> {
        self.map(|value| (true, value))
    }
}

impl<T, E> RepeatResult for Result<T, E> {
    type RepeatRes = Result<T, E>;

    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)> {
        match self {
            Ok(value) => Some((true, Ok(value))),
            Err(err) => Some((false, Err(err))),
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum RepFlow<T> {
    Continue(T),
    Finish(T),
    Reject,
}

impl<T> LookaheadResult for RepFlow<T> {
    fn should_commit(&self) -> bool {
        match self {
            Self::Reject => false,
            _ => true,
        }
    }
}

impl<T> RepeatResult for RepFlow<T> {
    type RepeatRes = T;

    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)> {
        match self {
            Self::Continue(value) => Some((true, value)),
            Self::Finish(value) => Some((false, value)),
            Self::Reject => None,
        }
    }
}

pub struct ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: LookaheadResult + RepeatResult,
{
    _ty: PhantomData<fn() -> O>,
    handler: F,
    reader: &'a mut R,
    finished: bool,
}

impl<'a, R, F, O> ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: LookaheadResult + RepeatResult,
{
    pub fn new(reader: &'a mut R, handler: F) -> Self {
        Self {
            _ty: PhantomData,
            handler,
            reader,
            finished: false,
        }
    }
}

impl<'a, R, F, O> Iterator for ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: LookaheadResult + RepeatResult,
{
    type Item = <O as RepeatResult>::RepeatRes;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let res = self.reader.lookahead(&mut self.handler);

        match res.into_stream_result() {
            Some((true, res)) => Some(res),
            Some((false, res)) => {
                self.finished = true;
                Some(res)
            }
            None => None,
        }
    }
}

impl<'a, R, F, O> Drop for ConsumeWhileIter<'a, R, F, O>
where
    R: LookaheadReader,
    F: FnMut(&mut R) -> O,
    O: LookaheadResult + RepeatResult,
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

impl<Reader, FDel, Res> LookaheadReader for DelimiterMatcher<Reader, FDel, Res> where FDel: Clone {}

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

// ...not sure why this is needed but type inference prefers it so ¯\_(ツ)_/¯
#[doc(hidden)]
pub fn helper_call_closure<F, A, R>(fn_: F, arg: A) -> R
where
    F: FnOnce(A) -> R,
{
    fn_(arg)
}

#[doc(hidden)]
pub fn helper_assert_equal_types<T: ?Sized>(_: &T, _: &T) {}

pub macro match_choice {
    (
        $reader:expr,
        $(|$binding:ident| $expr:expr),*$(,)?
    ) => {
        match_choice!($reader, [$(|$binding| $expr),*])
    },
    (
        $reader:expr,
        $([
            $(|$binding:ident| $expr:expr),*$(,)?
        ]),*$(,)?
    ) => {
        // This loop allows us to short-circuit early if one of the inner groups succeeds.
        loop {
            let reader = $reader;
            let mut error = ErrorAccumulator::empty_error();

            // For every atomic block...
            $({
                // Keep track of the current successes.
                let mut result = None;

                // For every pattern...
                $({
                    // Fork the reader
                    let mut fork = Clone::clone(reader);

                    // And lookahead
                    let fork_res = helper_call_closure(|$binding| $expr, &mut fork);

                    // This hint tells Rust inference that errors and results are of the same type
                    // so that we can call "extend_error" without inference issues.
                    // TODO: Is this really necessary?
                    helper_assert_equal_types(&error, &fork_res);

                    // Check if the pattern was matched successfully.
                    if LookaheadResult::should_commit(&fork_res) {
                        // If it was, ensure that it's not conflicting with anything else in the block.
                        debug_assert!(result.is_none(), "Ambiguous pattern!");

                        // And mark this as the one result to be returned at the end of the block if
                        // everything goes to plan.
                        result = Some((fork, fork_res));
                    } else {
                        // Otherwise, extend the error.
                        ErrorAccumulator::extend_error(&mut error, fork_res);
                    }
                })*

                // If we matched anything in the block, commit the reader and return it.
                if let Some((fork, result)) = result {
                    *reader = fork;
                    break result;
                }
            })*

            // If none of the blocks ever matched, return an accumulated error.
            break error;
        }
    }
}

// === Generic stream machinery === //

pub trait StreamReader: Sized {
    type Res: StreamResult;

    fn consume(&mut self) -> Self::Res;

    fn as_drain(&mut self) -> StreamDrain<'_, Self> {
        StreamDrain { reader: self }
    }

    fn peek(&self) -> Self::Res
    where
        Self: LookaheadReader,
    {
        self.clone().consume()
    }

    fn has_remaining(&self) -> bool
    where
        Self: LookaheadReader,
    {
        self.peek().is_present()
    }
}

#[derive(Debug)]
pub struct StreamDrain<'a, R> {
    pub reader: &'a mut R,
}

impl<'a, R: ?Sized + StreamReader> Iterator for StreamDrain<'a, R> {
    type Item = <R::Res as StreamResult>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.consume().to_item()
    }
}

pub trait StreamResult: Sized {
    type Item;

    fn to_item(self) -> Option<Self::Item>;

    fn is_present(self) -> bool {
        self.to_item().is_some()
    }

    fn is_end(self) -> bool {
        self.to_item().is_none()
    }
}

impl<T> StreamResult for Option<T> {
    type Item = T;

    fn to_item(self) -> Option<Self::Item> {
        self
    }
}

impl<T, E> StreamResult for Result<Option<T>, E> {
    type Item = Result<T, E>;

    fn to_item(self) -> Option<Self::Item> {
        match self {
            Ok(Some(success)) => Some(Ok(success)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }
}
