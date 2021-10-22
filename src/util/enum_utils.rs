use std::hash::Hash;
use std::slice::Iter as SliceIter;

// === Enum Metadata === //

pub trait EnumMeta: 'static + Sized + Copy + Eq + Hash {
    type Meta: 'static;

    fn values() -> &'static [(Self, Self::Meta)];
    fn meta(self) -> &'static Self::Meta;

    fn values_iter() -> EnumMetaIter<Self> {
        EnumMetaIter::from_slice(Self::values())
    }

    fn find_where<F>(mut fn_: F) -> Option<Self>
    where
        F: FnMut(Self, &'static Self::Meta) -> bool,
    {
        Self::values_iter().find_map(|(val, meta)| if fn_(val, meta) { Some(val) } else { None })
    }
}

#[derive(Clone)]
pub struct EnumMetaIter<T: EnumMeta> {
    iter: SliceIter<'static, (T, T::Meta)>,
}

impl<T: EnumMeta> EnumMetaIter<T> {
    pub fn from_slice(slice: &'static [(T, T::Meta)]) -> Self {
        Self { iter: slice.iter() }
    }

    fn map_item(entry: &(T, T::Meta)) -> (T, &T::Meta) {
        (entry.0, &entry.1)
    }
}

impl<T: EnumMeta> ExactSizeIterator for EnumMetaIter<T> {}
impl<T: EnumMeta> Iterator for EnumMetaIter<T> {
    type Item = (T, &'static T::Meta);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next().map(Self::map_item)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.iter.len();
        (len, Some(len))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        self.iter.len()
    }

    fn last(self) -> Option<Self::Item>
    where
        Self: Sized,
    {
        self.iter.last().map(Self::map_item)
    }

    fn nth(&mut self, n: usize) -> Option<Self::Item> {
        self.iter.nth(n).map(Self::map_item)
    }
}

pub macro enum_meta($(
    $(#[$item_attr:meta])*
    $vis:vis enum($meta_ty:ty) $item_name:ident {
        $(
			$(#[$var_attr:meta])*  // Also accepts doc comments, which are transformed into attributes during tokenization.
			$var_name:ident = $meta:expr
		),*
		$(,)?
    }
)*) {$(
    $(#[$item_attr])*
    #[derive(Copy, Clone, Eq, PartialEq, Hash)]
    $vis enum $item_name {$(
        $(#[$var_attr])*
        $var_name
	),*}

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

pub trait EnumDiscriminant {
    type Discriminant;

    fn discriminant(&self) -> Self::Discriminant;
}

impl EnumDiscriminant for u8 {
    type Discriminant = u8;

    fn discriminant(&self) -> Self::Discriminant {
        *self
    }
}

pub trait EnumMetaDiscriminantExt: EnumMeta
where
    Self::Meta: EnumDiscriminant<Discriminant = Self::Discriminant>,
{
    type Discriminant: Sized + Hash + Eq;

    fn to_disc(&self) -> Self::Discriminant;
    fn try_from_disc(discriminant: Self::Discriminant) -> Option<Self>;
    fn from_disc(discriminant: Self::Discriminant) -> Self {
        Self::try_from_disc(discriminant).unwrap()
    }
}

impl<D, M, T> EnumMetaDiscriminantExt for T
where
    D: Sized + Copy + Hash + Eq,
    M: 'static + EnumDiscriminant<Discriminant = D>,
    T: EnumMeta<Meta = M>,
{
    type Discriminant = D;

    fn to_disc(&self) -> Self::Discriminant {
        self.meta().discriminant()
    }

    fn try_from_disc(discriminant: Self::Discriminant) -> Option<Self> {
        Self::find_where(move |_, meta| discriminant == meta.discriminant())
    }
}

// === Object categories === //

pub trait ObjectCategoryExt: Sized {
    fn new<T: VariantOf<Self>>(value: T) -> Self {
        value.wrap()
    }

    fn try_cast<T: VariantOf<Self>>(self) -> Result<T, Self> {
        T::match_owned(self)
    }

    fn cast<T: VariantOf<Self>>(self) -> T {
        self.try_cast().ok().unwrap()
    }

    fn try_cast_ref<T: VariantOf<Self>>(&self) -> Option<&T> {
        T::match_ref(self)
    }

    fn cast_ref<T: VariantOf<Self>>(&self) -> &T {
        self.try_cast_ref().unwrap()
    }

    fn try_cast_mut<T: VariantOf<Self>>(&mut self) -> Option<&mut T> {
        T::match_mut(self)
    }

    fn cast_mut<T: VariantOf<Self>>(&mut self) -> &mut T {
        self.try_cast_mut().unwrap()
    }
}

pub trait VariantOf<E>: Sized {
    fn wrap(self) -> E;
    fn match_owned(e: E) -> Result<Self, E>;
    fn match_ref(e: &E) -> Option<&Self>;
    fn match_mut(e: &mut E) -> Option<&mut Self>;
}

pub macro enum_categories($(
    $(#[$item_attr:meta])*
    $vis:vis enum $item_name:ident {
        $(
			$(#[$var_attr:meta])*  // Also accepts doc comments, which are transformed into attributes during tokenization.
			$var_name:ident$(($var_ty:ty))?
		),*
		$(,)?
    }
)*) {$(
    $(#[$item_attr])*
    $vis enum $item_name {$(
        $(#[$var_attr])*
        $var_name$(($var_ty))?
	),*}

    impl ObjectCategoryExt for $item_name {}

    $($(
    impl VariantOf<$item_name> for $var_ty {
        fn wrap(self) -> $item_name {
            $item_name::$var_name(self)
        }

        fn match_owned(e: $item_name) -> Result<Self, $item_name> {
            match e {
                $item_name::$var_name(val) => Ok(val),
                e @ _ => Err(e),
            }
        }

        fn match_ref(e: &$item_name) -> Option<&Self> {
            match e {
                $item_name::$var_name(val) => Some(val),
                _ => None,
            }
        }

        fn match_mut(e: &mut $item_name) -> Option<&mut Self> {
            match e {
                $item_name::$var_name(val) => Some(val),
                _ => None,
            }
        }
    }
    )?)*
)*}
