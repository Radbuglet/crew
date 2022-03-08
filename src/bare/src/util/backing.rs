pub trait Take<T>: Sized {
    fn take_owned(self) -> T;
    fn take_ref(&self) -> &T;
}

impl<T> Take<T> for T {
    fn take_owned(self) -> T {
        self
    }

    fn take_ref(&self) -> &T {
        self
    }
}

impl<T: Clone> Take<T> for &'_ T {
    fn take_owned(self) -> T {
        self.clone()
    }

    fn take_ref(&self) -> &T {
        *self
    }
}

pub trait Captures<'a> {}

impl<'a, T: ?Sized> Captures<'a> for T {}
