use hashbrown::raw::RawTable;
use std::collections::hash_map::{DefaultHasher, RandomState};
use std::hash::{BuildHasher, Hash, Hasher};
use std::sync::atomic::{AtomicU64, Ordering};

type StringHashBuilder = RandomState;
type StringHasher = DefaultHasher;

#[cfg(debug_assertions)]
static INTERN_SET_ID: AtomicU64 = AtomicU64::new(0);

pub struct Interner {
    // A map from string hashes to offsets where equality can be checked against the `store`.
    lookup: RawTable<TableEntry>,

    // The [BuildHasher] for `lookup`.
    hash_builder: StringHashBuilder,

    // A single buffer storing all interned strings.
    // Characters from `head` to the end of the string are used to store temporary strings that may
    // or may not be interned.
    store: String,

    // A debug-only identifier to detect when an [Intern] is used on a non-owning [Interner].
    #[cfg(debug_assertions)]
    set: u64,
}

#[derive(Debug, Copy, Clone)]
struct TableEntry {
    hash: u64,
    offset: usize,
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

impl Interner {
    pub fn new() -> Self {
        Self {
            lookup: RawTable::new(),
            hash_builder: StringHashBuilder::new(),
            store: String::new(),
            #[cfg(debug_assertions)]
            // Memory access order does not depend on the value fetched here.
            set: INTERN_SET_ID.fetch_add(1, Ordering::Relaxed),
        }
    }

    pub fn begin_intern(&mut self) -> InternBuilder {
        let start = self.store.len();
        let hasher = self.hash_builder.build_hasher();

        InternBuilder {
            interner: self,
            hasher,
            start,
        }
    }

    pub fn intern(&mut self, str: &str) -> Intern {
        let mut builder = self.begin_intern();
        builder.push_str(str);
        builder.build()
    }

    pub fn resolve(&self, interned: Intern) -> &str {
        debug_assert_eq!(
            self.set, interned.set,
            "Tried to resolve an interned string from the wrong interner!"
        );
        self.resolve_slice(interned.start, interned.len)
    }

    fn resolve_slice(&self, start: usize, len: usize) -> &str {
        &self.store[start..(start + len)]
    }
}

pub struct InternBuilder<'a> {
    interner: &'a mut Interner,
    hasher: StringHasher,
    start: usize,
}

impl InternBuilder<'_> {
    pub fn push(&mut self, char: char) {
        self.interner.store.push(char);
        char.hash(&mut self.hasher);
    }

    pub fn push_str(&mut self, str: &str) {
        self.interner.store.push_str(str);
        for char in str.chars() {
            char.hash(&mut self.hasher);
        }
    }

    pub fn as_str(&self) -> &str {
        &self.interner.store[self.start..]
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }

    pub fn build(mut self) -> Intern {
        // Get string codepoint length
        let intern_str = self.as_str();
        let len = self.interner.store.len() - self.start;

        // Build string hash
        let hash = self.hasher.finish();

        // Attempt to lookup an existing entry
        let entry = self.interner.lookup.get(hash, |entry| {
            // Compare hash (fast-path)
            if hash != entry.hash {
                return false;
            }

            // Compare strings (slow-path, amortized by the interner)
            let entry_str = self.interner.resolve_slice(entry.offset, len);
            if intern_str != entry_str {
                return false;
            }

            true
        });

        // Build intern
        if let Some(entry) = entry {
            let intern = Intern {
                start: entry.offset,
                len,
                set: self.interner.set,
            };
            self.discard_tmp();
            intern
        } else {
            log::trace!("Interned \"{}\" with hash {}", intern_str, hash);

            let intern = Intern {
                start: self.start,
                len,
                set: self.interner.set,
            };
            self.interner.lookup.insert(
                hash,
                TableEntry {
                    hash,
                    offset: self.start,
                },
                |entry| entry.hash,
            );
            self.start = self.interner.store.len();
            intern
        }
    }

    fn discard_tmp(&mut self) {
        self.interner.store.truncate(self.start);
    }
}

impl Drop for InternBuilder<'_> {
    fn drop(&mut self) {
        self.discard_tmp();
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Intern {
    start: usize,
    len: usize,
    #[cfg(debug_assertions)]
    set: u64,
}

impl Hash for Intern {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.start)
    }
}

impl Eq for Intern {}

impl PartialEq for Intern {
    fn eq(&self, other: &Self) -> bool {
        debug_assert_eq!(
            self.set, other.set,
            "Interned strings must come from the same set!"
        );
        self.start == other.start
    }
}
