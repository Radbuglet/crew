use self::bump::BumpAlloc;
use self::rc::{CustomRc, CustomWeak};

pub mod bump;
pub mod intern;
pub mod rc;

pub type BBox<T> = Box<T, BumpAlloc>;
pub type BRc<T> = CustomRc<T, BumpAlloc>;
pub type BWeak<T> = CustomWeak<T, BumpAlloc>;
pub type BVec<T> = Vec<T, BumpAlloc>;
