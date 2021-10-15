// === Interface === //

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

pub trait StreamReader {
    type Res: StreamResult;

    fn consume(&mut self) -> Self::Res;

    fn as_consumer(&mut self) -> StreamConsumer<'_, Self> {
        StreamConsumer { reader: self }
    }

    fn peek(&self) -> Self::Res
    where
        Self: LookaheadReader,
    {
        self.clone().consume()
    }

    fn as_iter(&self) -> StreamIter<'_, Self> {
        StreamIter { reader: self }
    }
}

#[derive(Debug)]
pub struct StreamConsumer<'a, R: ?Sized> {
    pub reader: &'a mut R,
}

impl<'a, R: ?Sized + StreamReader> Iterator for StreamConsumer<'a, R> {
    type Item = <R::Res as StreamResult>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.consume().to_item()
    }
}

#[derive(Debug)]
pub struct StreamIter<'a, R: ?Sized> {
    pub reader: &'a R,
}

impl<R> Clone for StreamIter<'_, R> {
    fn clone(&self) -> Self {
        Self {
            reader: self.reader,
        }
    }
}

impl<'a, R: ?Sized + StreamReader + LookaheadReader> Iterator for StreamIter<'a, R> {
    type Item = <R::Res as StreamResult>::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.peek().to_item()
    }
}

pub trait StreamResult {
    type Item;

    fn to_item(self) -> Option<Self::Item>;
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
