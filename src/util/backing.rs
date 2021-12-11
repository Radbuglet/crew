pub trait Take<T>: Sized {
    fn as_ref(&self) -> &T;
    fn take(self) -> T;
}

impl<T> Take<T> for T {
    fn as_ref(&self) -> &T {
        self
    }

    fn take(self) -> T {
        self
    }
}

impl<T: Clone> Take<T> for &'_ T {
    fn as_ref(&self) -> &T {
        *self
    }

    fn take(self) -> T {
        self.clone()
    }
}

pub trait Captures<'a> {}

impl<'a, T: ?Sized> Captures<'a> for T {}
