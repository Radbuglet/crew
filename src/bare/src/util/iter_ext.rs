use std::iter::FromIterator;
use std::mem::MaybeUninit;
use std::ops::{ControlFlow, FromResidual, Try};

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

// === "Tests" === //

#[test]
fn swap_example() {
    let mut elems = vec![1, 2, 3, 4];
    let [a, b] = elems.iter_mut().collect_array();
    std::mem::swap(a, b);
    println!("{:?}", elems);
}
