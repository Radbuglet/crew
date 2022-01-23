use std::marker::PhantomData;

// === Lookahead machinery === //

/// Readers are cursors which allow users to procedurally match a grammar. [LookaheadReaders] are
/// special because they allow users to fork the cursor and "look-ahead" an arbitrary number of
/// elements to see if the grammar matches.
///
/// Match functions map fairly well to the notion of [production rules] but have one important
/// additional responsibility. In theory, while the entire grammar of a language could be built up of
/// silently matching production rules with the validity of the root-most rule determining whether the
/// program is valid, the user would loose context as to why their syntax was invalid. Thus, match
/// functions have the additional responsibility of detecting the user's intention, yielding
/// appropriate diagnostic messages on a grammar mismatch, and recovering to a safer state to continue
/// parsing.
///
/// ## Callbacks over Composition
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
/// There are two contexts in which a path can be used: a `use` item where each path can be ended
/// with a `"::" + <terminator>` and in expressions where this is not possible. It's tempting to
/// construct a match function that just matches this first "simple path" part and then compose that
/// with another function to match the optional terminator, however doing so complicates diagnostic
/// handling.
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
///  //               ^ the error is technically the path not being followed by a `:: + <group>` or a `;`.
///  //                 ^ however, the actual list of valid grammatical elements should also include `idents`.
///
/// static val foo: bar::baz:: = ...;
/// //                      ^ the error is technically the simple path not being followed by a `=` or `;`
/// //                         ^ however, the error is intuitively the lack of an identifier after the turbo.
/// ```
///
/// To fix this, matcher functions can take matcher closures to extend their grammar. In this case,
/// the `match_simple_path` can accept a closure taking a mutable [LookaheadReader] reference which
/// will provide an additional way to match an unclosed terminator. This both enables the primary
/// function to detect illegal terminators (since the closure could produce the appropriate error in
/// the correct context) and would allow the grammar extensions to work off context from the parent
/// function which would have otherwise been discarded.
///
/// ## Cursor Recovery
///
/// Some [LookaheadReaders] must occasionally do more than just report precise errors. Part of
/// implementing good diagnostics is making sure that the compiler can reasonably recover from the
/// error and attempt to analyze the rest of the project so that the user can fix as many errors as
/// possible from a single compilation's diagnostics. Part of error recovery is ensuring that the
/// cursor can proceed past the illegal grammatical element, what we call "cursor recovery". Deciding
/// when to attempt to recover from a grammatical mismatch is similar to the decision of whether to
/// fail silently or to produce a diagnostic message. Once the match function detects a syntactical
/// error in the middle of the input and the user's intent can be unambiguously traced to that
/// grammatical rule, the implementor can attempt to consume syntactic atoms until the reader is at
/// the start of the next likely grammatical element.
///
/// Here are a few examples of how this would work in statement parsing:
///
/// ```no_run
/// let foo = ;
/// //        ^ we failed to match the expected expression.
/// //          Let's consume to the semicolon and continue parsing.
/// let bar baz = 4;
/// //      ^ we got a second identifier.
/// //        Let's try to consume to the equality sign so we can continue parsing the expression.
/// //        If that fails, let's consume until we reach either the semicolon or the EOF.
///
/// let foo 4;
/// //      ^ we're missing the equality punct.
/// //        Let's try to match everything up until the semicolon or the EOF as an expression.
///
/// let foo = 4 let next_statement = 4;
/// //          ^ we get an unexpected keyword here while parsing the expression.
/// //            We can interrupt the expression atomization phase on this error and use everything
/// //            left of the statement keyword as the expression and use the keyword as the next
/// //            token to return once the parse function is recovered.
/// ```
///
/// Alternatively, match functions could accept closures for callbacks specifically for error handling
/// and then propagate the function's return result. That way, users can decide whether to treat the
/// error as a hard error or attempt cursor recovery with the grammatical parent's additional context.
///
/// The [lookahead] method commits or discards the cursor state depending on the "truthiness" of the
/// closure's return value. It may seem weird to return a `Some(Err(...))` to represent a recovered
/// error so it may be useful to think of the root-level return value of a match function as
/// indicating syntactic *intent* instead of syntactic *validity*.
///
/// [LookaheadReaders]: LookaheadReader
/// [production rules]: https://en.wikipedia.org/wiki/Production_(computer_science)
/// [lookahead]: LookaheadReader::lookahead
pub trait LookaheadReader: Clone {
    /// Attempts to match the reader sequence using the handler, committing the state if the return
    /// value is truthy (*e.g.* `true`, `Some(_)`, `Ok(_)`; see [LookaheadResult] for details) and
    /// ignoring all reader state changes otherwise.
    fn lookahead<F, R>(&mut self, handler: F) -> R::LookaheadRes
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        self.lookahead_raw(handler).into_result()
    }

    /// Same behavior as [lookahead] except it doesn't unwrap the returned [LookaheadResult].
    fn lookahead_raw<F, R>(&mut self, handler: F) -> R
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

    /// Forks the reader and peaks ahead without ever committing the result. Will unwrap any returned
    /// [LookaheadResult]s.
    fn peek_ahead<F, R>(&self, handler: F) -> R::LookaheadRes
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.clone()).into_result()
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

/// A [LookaheadResult] tells [LookaheadReader::lookahead] whether the lookahead matched and can
/// unwrap itself into a simpler form for consumers of that lookahead.
pub trait LookaheadResult {
    type LookaheadRes;

    fn should_commit(&self) -> bool;
    fn into_result(self) -> Self::LookaheadRes;
}

impl LookaheadResult for bool {
    type LookaheadRes = Self;

    fn should_commit(&self) -> bool {
        *self
    }

    fn into_result(self) -> Self::LookaheadRes {
        self
    }
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

impl<T> LookaheadResult for Option<T> {
    type LookaheadRes = Self;

    fn should_commit(&self) -> bool {
        self.is_some()
    }

    fn into_result(self) -> Self::LookaheadRes {
        self
    }
}

impl<T> RepeatResult for Option<T> {
    type RepeatRes = T;

    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)> {
        self.map(|value| (true, value))
    }
}

pub type PResult<T, E> = Result<Option<T>, E>;
pub type BarePResult<T> = PResult<T, ()>;

impl<T, E> LookaheadResult for PResult<T, E> {
    type LookaheadRes = Self;

    fn should_commit(&self) -> bool {
        match self {
            Ok(Some(_)) => true,
            Ok(None) => false,
            Err(_) => true,
        }
    }

    fn into_result(self) -> Self {
        self
    }
}

pub fn invert_p_result<T, E>(result: PResult<T, E>) -> Option<Result<T, E>> {
    match result {
        Ok(Some(present)) => Some(Ok(present)),
        Ok(None) => None,
        Err(err) => Some(Err(err)),
    }
}

// === Repetition matching === //

/// A [RepeatResult] tells a [ConsumeWhileIter] two things:
///
/// 1. Whether it should continue iterating or return a value.
/// 2. Whether to commit the lookahead for that specific iteration (inherited from [LookaheadResult]).
///
pub trait RepeatResult: Sized + LookaheadResult {
    type RepeatRes;

    /// Attempts to unwrap the result into the value returned by the iterator. The first element
    /// of the tuple indicates whether to attempt to continue iterating. The iterator will stop
    /// regardless if the result is `None`.
    fn into_stream_result(self) -> Option<(bool, Self::RepeatRes)>;
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum RepFlow<T> {
    Continue(T),
    Finish(T),
    Reject,
}

impl<T> LookaheadResult for RepFlow<T> {
    type LookaheadRes = Self;

    fn should_commit(&self) -> bool {
        match self {
            Self::Reject => false,
            _ => true,
        }
    }

    fn into_result(self) -> Self::LookaheadRes {
        self
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

        let res = self.reader.lookahead_raw(&mut self.handler);

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
    Res: LookaheadResult,
    Res::LookaheadRes: Default,
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

    pub fn next(&mut self, reader: &mut Reader) -> Option<Res::LookaheadRes> {
        if self.first {
            self.first = false;
            Some(Default::default())
        } else {
            let res = reader.lookahead_raw(&mut self.match_del);
            if res.should_commit() {
                Some(res.into_result())
            } else {
                None
            }
        }
    }
}

// === Branch matching === //

// ...not sure why this is needed but type inference prefers it so ¯\_(ツ)_/¯
pub fn helper_call_closure<F, A, R>(fn_: F, arg: A) -> R
where
    F: FnOnce(A) -> R,
{
    fn_(arg)
}

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
        loop {
            let reader = $reader;

            $({
                let mut result = None;

                $({
                    let mut fork = Clone::clone(reader);
                    if let Some(res) = helper_call_closure(|$binding| $expr, &mut fork) {
                        debug_assert!(result.is_none(), "Ambiguous pattern!");
                        result = Some((fork, res));
                    }
                })*

                if let Some((fork, result)) = result {
                    *reader = fork;
                    break Some(result);
                }
            })*

            break None;
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
