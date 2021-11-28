use crate::util::alloc::bump::BumpAlloc;
use hashbrown::raw::RawTable;
use std::collections::hash_map::RandomState;
use std::fmt::{Display, Formatter};
use std::hash::BuildHasher;
use std::ops::Deref;
use std::slice::from_raw_parts;
use std::str::from_utf8_unchecked as str_from_utf8_unchecked;

pub struct Interner {
    map: RawTable<MapEntry>,
    hash_builder: RandomState,
    alloc: BumpAlloc,
}

struct MapEntry {
    hash: u64,
    // Safety: this `str` is being kept alive by `Interner.alloc`.
    str: &'static str,
}

impl Interner {
    pub fn new(alloc: BumpAlloc) -> Self {
        Self {
            map: RawTable::new(),
            hash_builder: RandomState::new(),
            alloc,
        }
    }

    pub fn builder(&mut self) -> InternBuilder {
        InternBuilder::new(self.alloc.clone())
    }

    pub fn find_entry(&self, text: &str) -> Option<Intern> {
        let entry = self.find_entry_raw(self.hash_builder.hash_one(text), text)?;
        Some(Intern {
            str: entry.str,
            alloc: self.alloc.clone(),
        })
    }

    fn find_entry_raw(&self, hash: u64, text: &str) -> Option<&MapEntry> {
        self.map
            .get(hash, |entry| entry.hash == hash && entry.str == text)
    }

    pub fn push_str(&mut self, str: &str) -> Intern {
        let mut builder = self.builder();
        builder.push_str(str);
        builder.build(self)
    }
}

#[derive(Debug)]
pub struct InternBuilder {
    bytes: Vec<u8, BumpAlloc>,
}

impl InternBuilder {
    pub fn new(alloc: BumpAlloc) -> Self {
        Self {
            bytes: Vec::new_in(alloc),
        }
    }

    pub fn push_char(&mut self, char: char) {
        let start = self.bytes.len();
        self.bytes.resize(start + char.len_utf8(), 0);
        char.encode_utf8(&mut self.bytes[start..]);
    }

    pub fn push_str(&mut self, str: &str) {
        self.bytes.extend(str.bytes());
    }

    pub fn as_str(&self) -> &str {
        unsafe { str_from_utf8_unchecked(&self.bytes) }
    }

    pub fn to_vec(self) -> Vec<u8, BumpAlloc> {
        self.bytes
    }

    pub fn extract_alloc(self) -> BumpAlloc {
        // Extract the raw parts
        let (ptr, len, cap, alloc) = self.bytes.into_raw_parts_with_alloc();

        // Drop the buffer using a temporary constructed by an allocator reference.
        drop(unsafe { Vec::from_raw_parts_in(ptr, len, cap, &alloc) });

        alloc
    }

    pub fn build(mut self, interner: &mut Interner) -> Intern {
        // Hash the new intern once.
        let hash = interner.hash_builder.hash_one(self.as_str());

        // Search for an existing intern
        if let Some(entry) = interner.find_entry_raw(hash, self.as_str()) {
            Intern {
                str: entry.str,
                alloc: self.extract_alloc(),
            }
        } else {
            // Otherwise, transform this existing vector into a new intern and register it.

            // Extract the allocator and a minimal str buffer
            self.bytes.shrink_to_fit();
            let (ptr, len, _, alloc) = self.bytes.into_raw_parts_with_alloc();
            let str = unsafe { str_from_utf8_unchecked(from_raw_parts(ptr, len)) };

            // Register it into the table
            interner
                .map
                .insert(hash, MapEntry { hash, str }, |entry| entry.hash);

            Intern { str, alloc }
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Intern {
    // Safety: this `str` is being kept alive by `self.alloc`.
    str: &'static str,
    alloc: BumpAlloc,
}

impl Intern {
    pub fn as_str(&self) -> &str {
        self.str
    }

    pub fn alloc(&self) -> &BumpAlloc {
        &self.alloc
    }
}

impl Display for Intern {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Deref for Intern {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}
