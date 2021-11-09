use std::marker::PhantomData;

// === Interface === //

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
/// with another function to match the optional terminator. However, this complicates diagnostic
/// handling.
///
/// In a non-composed context, we could say that turbo (`::`) delimiters must be followed by either
/// a non-keyword identifier or a braced group of nested paths. However, in allowing users to define
/// syntax which overlaps with the "unambiguous" section of the grammar, we must now silently accept
/// trailing terminators. While this shouldn't enable any new illegal grammar (otherwise, the grammar
/// would have been ambiguous), the diagnostics become much less contextual:
///
/// ```no_run
///  use foo::bar::baz::;
///  //               ^ the error is technically the turbo not being a semicolon.
///  //                 ^ however, the error is intuitively the lack of a valid pattern after the turbo.
///
///  fn my_func() -> u32 {
///  // Here, this expression is parsed as two separate paths:
///      maz::laz::SOME_STATIC::crate + 5
///  //  ^ path 1             ^ path 2
///  // This happens because "crate" is only valid at the start of a simple path.
///  // The error will now happen during expression folding instead of during atomization.
///  }
/// ```
///
/// To fix this, matcher functions can take matcher closures to extend their grammar. In this case,
/// the `match_simple_path` can accept a closure taking a mutable [LookaheadReader] reference which
/// will provide an additional way to match an unclosed terminator. This both enables the primary
/// function to detect illegal terminators (instead of ignoring them) and allows the grammar
/// extensions to work off context from the parent function which would otherwise be discarded.
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
    fn lookahead<F, R>(&mut self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        self.lookahead_raw(handler).into_result()
    }

    fn lookahead_raw<F, R>(&mut self, handler: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        let mut lookahead = self.clone();
        let res = handler(&mut lookahead);
        if res.is_truthy() {
            *self = lookahead;
        }
        res
    }

    fn peek_ahead<F, R>(&self, handler: F) -> R::Ret
    where
        F: FnOnce(&mut Self) -> R,
        R: LookaheadResult,
    {
        handler(&mut self.clone()).into_result()
    }

    fn consume_while<F, R>(&mut self, mut handler: F) -> usize
    where
        F: FnMut(&mut Self) -> R,
        R: LookaheadResult,
        R::Ret: RepFlowIndicator,
    {
        let mut found = 0;
        loop {
            let res = self.lookahead_raw(&mut handler);

            // Only count successful matches
            if res.is_truthy() {
                found += 1;
            }

            // Only continue if asked
            if !res.into_result().should_continue() {
                break;
            }
        }
        found
    }
}

pub macro match_choice($reader:expr, $(|$binding:ident| $expr:expr),*$(,)?) {{
    let reader = $reader;
    let mut result = None;
    $(
        if let Some(res) = reader.lookahead(|$binding| $expr) {
            assert!(result.is_none(), "Ambiguous pattern!");
            result = Some(res);
        }
    )*
    result
}}

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

pub trait RepFlowIndicator: Sized {
    fn should_continue(self) -> bool;
}

impl RepFlowIndicator for bool {
    fn should_continue(self) -> bool {
        self
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum RepFlow {
    Continue,
    Finish,
    Reject,
}

impl RepFlowIndicator for RepFlow {
    fn should_continue(self) -> bool {
        self == Self::Continue
    }
}

impl LookaheadResult for RepFlow {
    type Ret = Self;

    fn is_truthy(&self) -> bool {
        *self != Self::Reject
    }

    fn into_result(self) -> Self::Ret {
        self
    }
}

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

pub struct DelimiterMatcher<Reader, FDel, Res> {
    _sig: PhantomData<fn(&mut Reader) -> Res>,
    match_del: FDel,
    first: bool,
}

impl<Reader, FDel, Res> DelimiterMatcher<Reader, FDel, Res>
where
    Reader: LookaheadReader,
    FDel: FnMut(&mut Reader) -> Res,
    Res: LookaheadResult,
    Res::Ret: Default,
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

    pub fn next(&mut self, reader: &mut Reader) -> Option<Res::Ret> {
        if self.first {
            self.first = false;
            Some(Default::default())
        } else {
            let res = reader.lookahead_raw(&mut self.match_del);
            if res.is_truthy() {
                Some(res.into_result())
            } else {
                None
            }
        }
    }
}

// === Generic readers === //

#[derive(Debug, Clone)]
pub struct IterReader<I> {
    iter: I,
}

impl<I> IterReader<I> {
    pub fn new(iter: I) -> Self {
        Self { iter }
    }
}

impl<I: Iterator> StreamReader for IterReader<I> {
    type Res = Option<I::Item>;

    fn consume(&mut self) -> Self::Res {
        self.iter.next()
    }
}

impl<I: Clone> LookaheadReader for IterReader<I> {}
