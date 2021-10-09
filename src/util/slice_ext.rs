use std::mem::MaybeUninit;

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

#[test]
fn swap_example() {
    let mut elems = vec![1, 2, 3, 4];
    let [a, b] = elems.iter_mut().collect_array();
    std::mem::swap(a, b);
    println!("{:?}", elems);
}
