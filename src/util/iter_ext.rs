use std::mem::MaybeUninit;

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

pub trait ArrayCollectExt: Sized + Iterator {
    fn try_collect_array<const N: usize>(self) -> Option<[<Self as Iterator>::Item; N]>;

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
}

impl<I: Iterator> ArrayCollectExt for I {
    fn try_collect_array<const N: usize>(mut self) -> Option<[I::Item; N]> {
        let mut arr = MaybeUninit::<I::Item>::uninit_array::<N>();

        for slot in arr.iter_mut() {
            slot.write(match self.next() {
                Some(next) => next,
                None => return None,
            });
        }

        Some(unsafe { MaybeUninit::array_assume_init(arr) })
    }
}

pub trait ResultLike {
    type Value;
    type Error;

    fn to_result(self) -> Result<Self::Value, Self::Error>;
}

impl<T> ResultLike for Option<T> {
    type Value = T;
    type Error = ();

    fn to_result(self) -> Result<Self::Value, Self::Error> {
        match self {
            Some(ok) => Ok(ok),
            None => Err(()),
        }
    }
}

impl<T, E> ResultLike for Result<T, E> {
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

impl<E: ResultLike, I: Iterator<Item = E>> TryCollectExt for I {
    type Value = E::Value;
    type Error = E::Error;

    fn try_collect<C: FromIterator<Self::Value>>(self) -> Result<C, Self::Error> {
        let mut mapped = self.map(ResultLike::to_result);
        let iter = (&mut mapped)
            .take_while(Result::is_ok)
            // Default unwrap methods format the error on panic.
            .map(|res| match res {
                Ok(res) => res,
                Err(_) => unreachable!(),
            });

        let collection = iter.collect::<C>();
        match mapped.next() {
            Some(Ok(_)) => unreachable!(),
            Some(Err(err)) => Err(err),
            None => Ok(collection),
        }
    }
}

#[test]
fn swap_example() {
    let mut elems = vec![1, 2, 3, 4];
    let [a, b] = elems.iter_mut().collect_array();
    std::mem::swap(a, b);
    println!("{:?}", elems);
}
