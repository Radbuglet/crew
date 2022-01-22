use std::mem::MaybeUninit;

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

pub trait CollectableResult {
    type Value;
    type Error;

    fn to_result(self) -> Result<Self::Value, Self::Error>;
}

impl<T> CollectableResult for Option<T> {
    type Value = T;
    type Error = ();

    fn to_result(self) -> Result<Self::Value, Self::Error> {
        match self {
            Some(ok) => Ok(ok),
            None => Err(()),
        }
    }
}

impl<T, E> CollectableResult for Result<T, E> {
    type Value = T;
    type Error = E;

    fn to_result(self) -> Result<Self::Value, Self::Error> {
        self
    }
}

pub trait TryCollectExt: Sized + Iterator {
    type Value;
    type Error;

    fn try_collect<C: FromIterator<Self::Value>>(self) -> Result<C, Self::Error>;
}

impl<E: CollectableResult, I: Iterator<Item = E>> TryCollectExt for I {
    type Value = E::Value;
    type Error = E::Error;

    fn try_collect<C: FromIterator<Self::Value>>(mut self) -> Result<C, Self::Error> {
        // Collect all "Ok" elements.
        let mut iter_error = None;
        let ok_iter = std::iter::from_fn(|| {
            // Ensure that users don't consume more than one error.
            if iter_error.is_some() {
                return None;
            }

            // Transform errors into "None" and flag them.
            match self.next()?.to_result() {
                Ok(elem) => Some(elem),
                Err(error) => {
                    iter_error = Some(error);
                    None
                }
            }
        });
        let collection = ok_iter.collect::<C>();

        // Check if there are any trailing non-ok elements.
        match iter_error {
            Some(err) => Err(err),
            None => Ok(collection),
        }
    }
}

// === "Tests" === //

#[test]
fn swap_example() {
    let mut elems = vec![1, 2, 3, 4];
    let [a, b] = elems.iter_mut().collect_array();
    std::mem::swap(a, b);
    println!("{:?}", elems);
}
