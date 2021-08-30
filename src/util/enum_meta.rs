use std::hash::Hash;

pub trait EnumMeta: Sized + Copy + Eq + Hash {
    type Meta;

    fn values() -> &'static [(Self, Self::Meta)];
    fn meta(self) -> &'static Self::Meta;
}

pub macro enum_meta($(
    $(#[$attr:meta])*
    $vis:vis enum($meta_ty:ty) $item_name:ident {
        $($var_name:ident = $meta:expr),*$(,)?
    }
)*) {$(
    $(#[$attr])*
    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    pub enum $item_name {
        $($var_name),*
    }

    impl $item_name {
        const ITEMS: [(Self, $meta_ty); 0 $(+ { let _ = Self::$var_name; 1 })*] = [
            $((Self::$var_name, $meta)),*
        ];
    }

    impl EnumMeta for $item_name {
        type Meta = $meta_ty;

        fn values() -> &'static [(Self, Self::Meta)] {
            &Self::ITEMS
        }

        fn meta(self) -> &'static Self::Meta {
            for (var, meta) in Self::values() {
                if self == *var {
                    return meta;
                }
            }
            unreachable!()
        }
    }
)*}
