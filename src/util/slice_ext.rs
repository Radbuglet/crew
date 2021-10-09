use std::mem::{size_of, ManuallyDrop, MaybeUninit};

// Unlike the `transmute` intrinsic, `insane_transmute` does not statically require `size_of::<A>()`
// to be equal to `size_of::<B>()`. This is necessary for `collect_array` because Rust does not yet
// know how to prove that `size_of::<[MaybeUninit<T>; N]>` is equal to `size_of::<[T; N]>` for all `N`.
//
// We do, however, check this invariant at runtime in the hopes that the compiler will be smart enough
// to optimize it away and the results look promising (https://godbolt.org/z/Krf9nbW4z). The difference
// between these two methods of proof is akin to the difference between Rust's universal qualification
// validation and C++'s SFINAE validation.
unsafe fn insane_transmute<A, B>(from: A) -> B {
    assert_eq!(size_of::<A>(), size_of::<B>());

    union Transmute<A, B> {
        from: ManuallyDrop<A>,
        to: ManuallyDrop<B>,
    }

    let value = Transmute {
        from: ManuallyDrop::new(from),
    };

    // Safety: provided by caller
    ManuallyDrop::into_inner(value.to)
}

pub trait ArrayCollectExt: Iterator {
    fn collect_array<const N: usize>(self) -> [<Self as Iterator>::Item; N];
}

impl<I: Iterator> ArrayCollectExt for I {
    fn collect_array<const N: usize>(mut self) -> [I::Item; N] {
        let mut arr = MaybeUninit::<I::Item>::uninit_array::<N>();

        for slot in arr.iter_mut() {
            slot.write(match self.next() {
                Some(next) => next,
                None => panic!(
                    "Iterator must have at least {} element{}.",
                    N,
                    if N != 1 { "s" } else { "" }
                ),
            });
        }

        unsafe { insane_transmute::<[MaybeUninit<I::Item>; N], [I::Item; N]>(arr) }
    }
}

#[test]
fn swap_example() {
    let mut elems = vec![1, 2, 3, 4];
    let [a, b] = elems.iter_mut().collect_array();
    std::mem::swap(a, b);
    println!("{:?}", elems);
}
