use std::fmt;

use derive_where::derive_where;

use super::misc::{FmtIter, FxHashMap, HashBuilderExt};

cfgenius::define! {
    has_debug_printing = cfg(debug_assertions);
}

#[derive(Debug, Copy, Clone)]
#[derive_where(Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Intern {
    #[derive_where(skip)]
    text: cfgenius::cond! {
        if macro(has_debug_printing) {
            &'static str
        } else {
            ()
        }
    },
    id: u32,
}

#[derive(Default)]
pub struct Interner {
    buffer: String,
    intern_strings: FxHashMap<InternEntry, ()>,
    intern_entries: Vec<(usize, usize)>,
}

#[derive(Debug)]
struct InternEntry {
    hash: u64,
    intern: Intern,
    offset: usize,
    len: usize,
}

impl fmt::Debug for Interner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Interner")
            .field(
                "interns",
                &FmtIter(
                    self.intern_entries
                        .iter()
                        .map(|(offset, len)| &self.buffer[*offset..][..*len]),
                ),
            )
            .finish()
    }
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn begin_intern(&mut self) -> InternBuilder<'_> {
        InternBuilder::new(self)
    }

    pub fn intern(&mut self, str: &str) -> Intern {
        self.begin_intern().with_str(str).finish()
    }

    pub fn decode(&mut self, intern: Intern) -> &str {
        let (offset, len) = self.intern_entries[intern.id as usize];
        &self.buffer[offset..][..len]
    }
}

pub struct InternBuilder<'a> {
    interner: &'a mut Interner,
    start: usize,
}

impl fmt::Debug for InternBuilder<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("InternBuilder")
            .field("text", &self.text())
            .finish_non_exhaustive()
    }
}

impl<'a> InternBuilder<'a> {
    pub fn new(interner: &'a mut Interner) -> Self {
        let start = interner.buffer.len();
        Self { interner, start }
    }

    pub fn push(&mut self, ch: char) -> &mut Self {
        self.interner.buffer.push(ch);
        self
    }

    pub fn with(mut self, ch: char) -> Self {
        self.push(ch);
        self
    }

    pub fn push_str(&mut self, string: &str) -> &mut Self {
        self.interner.buffer.push_str(string);
        self
    }

    pub fn with_str(mut self, string: &str) -> Self {
        self.push_str(string);
        self
    }

    pub fn text(&self) -> &str {
        &self.interner.buffer[self.start..]
    }

    pub fn finish(self) -> Intern {
        let text = &self.interner.buffer[self.start..];
        let hash = self.interner.intern_strings.hasher().hash_one(text);

        match self
            .interner
            .intern_strings
            .raw_entry_mut()
            .from_hash(hash, |intern| {
                hash == intern.hash && text == &self.interner.buffer[intern.offset..][..intern.len]
            }) {
            hashbrown::hash_map::RawEntryMut::Occupied(entry) => {
                self.interner.buffer.truncate(self.start);
                entry.key().intern
            }
            hashbrown::hash_map::RawEntryMut::Vacant(entry) => {
                let offset = self.start;
                let len = self.interner.buffer.len() - self.start;

                let intern = Intern {
                    text: cfgenius::cond_expr! {
                        if macro(has_debug_printing) {
                            Box::leak(text.to_string().into_boxed_str())
                        } else {
                            ()
                        }
                    },
                    id: u32::try_from(self.interner.intern_entries.len())
                        .expect("too many interns"),
                };
                self.interner.intern_entries.push((offset, len));

                entry.insert_with_hasher(
                    hash,
                    InternEntry {
                        hash,
                        intern,
                        offset,
                        len,
                    },
                    (),
                    |entry| entry.hash,
                );

                intern
            }
        }
    }
}

impl Drop for InternBuilder<'_> {
    fn drop(&mut self) {
        self.interner.buffer.truncate(self.start);
    }
}
