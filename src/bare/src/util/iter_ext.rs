use std::iter::FromIterator;
use std::mem::MaybeUninit;
use std::ops::{ControlFlow, FromResidual, Try};

// === Generic Adapters === //

#[derive(Debug, Clone)]
pub struct Adapts<A, S> {
    adapter: A,
    source: S,
}

impl<V, S: Iterator<Item = V>, A: IteratorAdapter<V>> Iterator for Adapts<A, S> {
    type Item = A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.adapter.next_iter(&mut self.source)
    }
}

pub trait IteratorAdapterSingle<S>: Sized {
    type Item;

    fn next_single(&mut self, source: Option<S>) -> Option<Self::Item>;
}

impl<S, A: IteratorAdapterSingle<S>> IteratorAdapter<S> for A {
    type Item = A::Item;

    fn next_iter<I: IntoIterator<Item = S>>(&mut self, source: I) -> Option<Self::Item> {
        self.next_single(source.into_iter().next())
    }
}

pub trait IteratorAdapter<S>: Sized {
    type Item;

    fn next_iter<I: IntoIterator<Item = S>>(&mut self, source: I) -> Option<Self::Item>;

    fn adapt<TS: IntoIterator<Item = S>>(self, source: TS) -> Adapts<Self, TS::IntoIter> {
        Adapts {
            adapter: self,
            source: source.into_iter(),
        }
    }

    fn new_adapted<TS: IntoIterator<Item = S>>(source: TS) -> Adapts<Self, TS::IntoIter>
    where
        Self: Default,
    {
        Self::default().adapt(source)
    }
}

// === ControlFlowIter === //

pub type UniControlFlow<T> = ControlFlow<T, T>;

#[derive(Debug, Clone, Default)]
pub struct ControlFlowIter {
    finished: bool,
}

impl ControlFlowIter {
    pub fn new() -> Self {
        Self::default()
    }
}

impl<V> IteratorAdapterSingle<UniControlFlow<V>> for ControlFlowIter {
    type Item = V;

    fn next_single(&mut self, source: Option<UniControlFlow<V>>) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        match source {
            Some(UniControlFlow::Continue(value)) => Some(value),
            Some(UniControlFlow::Break(value)) => {
                self.finished = true;
                Some(value)
            }
            None => {
                self.finished = true;
                None
            }
        }
    }
}

pub trait ControlFlowIterExt: Sized {
    fn handle_control_flow(self) -> Adapts<ControlFlowIter, Self>;
}

impl<V, I: Iterator<Item = UniControlFlow<V>>> ControlFlowIterExt for I {
    fn handle_control_flow(self) -> Adapts<ControlFlowIter, Self> {
        ControlFlowIter::new_adapted(self)
    }
}

// === ControlFlowIter conversions === //

pub trait ToIterControlFlow: Sized {
    type Item;

    fn to_rep_flow(self) -> RepFlow<Self::Item>;

    fn to_control_flow_option(self) -> Option<UniControlFlow<Self::Item>> {
        match self.to_rep_flow() {
            RepFlow::Break(value) => Some(UniControlFlow::Break(value)),
            RepFlow::Continue(value) => Some(UniControlFlow::Continue(value)),
            RepFlow::None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum RepFlow<T> {
    Break(T),
    Continue(T),
    None,
}

impl<T> ToIterControlFlow for RepFlow<T> {
    type Item = T;

    fn to_rep_flow(self) -> RepFlow<Self::Item> {
        self
    }
}

impl ToIterControlFlow for bool {
    type Item = ();

    fn to_rep_flow(self) -> RepFlow<Self::Item> {
        match self {
            true => RepFlow::Continue(()),
            false => RepFlow::None,
        }
    }
}

impl<T> ToIterControlFlow for Option<T> {
    type Item = T;

    fn to_rep_flow(self) -> RepFlow<Self::Item> {
        match self {
            Some(value) => RepFlow::Continue(value),
            None => RepFlow::None,
        }
    }
}

impl<T, E> ToIterControlFlow for Result<T, E> {
    type Item = Self;

    fn to_rep_flow(self) -> RepFlow<Self::Item> {
        match self {
            Ok(value) => RepFlow::Continue(Ok(value)),
            Err(err) => RepFlow::Break(Err(err)),
        }
    }
}

// === TakeAt === //

pub trait TakeAtExt: Sized + Iterator {
    fn take_at<I: Iterator<Item = usize>>(self, indices: I) -> TakeAtIter<Self, I> {
        TakeAtIter {
            min_idx: 0,
            iter_src: self,
            iter_idx: indices,
        }
    }
}

impl<T: Sized + Iterator> TakeAtExt for T {}

#[derive(Debug, Clone)]
pub struct TakeAtIter<ISrc, IIdx> {
    min_idx: usize,
    iter_src: ISrc,
    iter_idx: IIdx,
}

impl<ISrc: Iterator, IIdx: Iterator<Item = usize>> Iterator for TakeAtIter<ISrc, IIdx> {
    type Item = ISrc::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(idx) = self.iter_idx.next() {
            assert!(
                idx >= self.min_idx,
                "TakeAtIter cannot go backwards ({} < {})",
                idx,
                self.min_idx
            );
            let item = self.iter_src.nth(idx - self.min_idx).unwrap();
            self.min_idx = idx + 1;
            Some(item)
        } else {
            None
        }
    }
}

// === ArrayCollect === //

pub trait ArrayCollectExt: Sized + Iterator {
    fn try_collect_array<const N: usize>(mut self) -> Option<[<Self as Iterator>::Item; N]> {
        let mut arr = MaybeUninit::<Self::Item>::uninit_array::<N>();

        for slot in arr.iter_mut() {
            slot.write(match self.next() {
                Some(next) => next,
                None => return None,
            });
        }

        Some(unsafe { MaybeUninit::array_assume_init(arr) })
    }

    fn collect_array<const N: usize>(self) -> [<Self as Iterator>::Item; N] {
        match self.try_collect_array() {
            Some(array) => array,
            None => panic!(
                "Iterator must have at least {} element{}.",
                N,
                if N != 1 { "s" } else { "" }
            ),
        }
    }

    fn collect_into(mut self, target: &mut [<Self as Iterator>::Item]) -> (usize, Self) {
        let mut filled = 0;
        while let Some(elem) = self.next() {
            if filled >= target.len() {
                return (filled, self);
            }
            target[filled] = elem;
            filled += 1;
        }
        (filled, self)
    }
}

impl<I: Iterator> ArrayCollectExt for I {}

// === TryCollect === //

pub trait TryCollectExt: Sized + Iterator
where
    Self::Item: Try,
{
    fn try_collect<R>(mut self) -> R
    where
        R: Try,
        R::Output: FromIterator<<Self::Item as Try>::Output>,
        R: FromResidual<<Self::Item as Try>::Residual>,
    {
        // Collect all "Ok" elements.
        let mut error_residual = None;
        let ok_iter = std::iter::from_fn(|| {
            // Ensure that users don't suddenly start getting success values after
            // an error has been encountered.
            if error_residual.is_some() {
                return None;
            }

            // Transform errors into "None" and flag them.
            match self.next()?.branch() {
                ControlFlow::Continue(value) => Some(value),
                ControlFlow::Break(residual) => {
                    error_residual = Some(residual);
                    None
                }
            }
        });

        let collection = ok_iter.collect::<R::Output>();

        // Check if there are any trailing non-ok elements.
        match error_residual {
            Some(err) => R::from_residual(err),
            None => R::from_output(collection),
        }
    }
}

impl<T> TryCollectExt for T
where
    T: Sized + Iterator,
    T::Item: Try,
{
}
