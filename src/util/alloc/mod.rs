// TODO: Finish and integrate

use self::bump::BumpAlloc;
use self::rc::{CustomRc, CustomWeak};

pub mod bump;
pub mod rc;

pub type BumpRc<T> = CustomRc<T, BumpAlloc>;
pub type BumpWeak<T> = CustomWeak<T, BumpAlloc>;
pub type BumpVec<T> = Vec<T, BumpAlloc>;
