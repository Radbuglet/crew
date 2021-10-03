// TODO: Implement actual interning logic

use std::fmt::{Display, Formatter, Result as FmtResult};

#[derive(Default)]
pub struct Interner {
    _private: (),
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn begin_intern(&mut self) -> InternBuilder {
        InternBuilder {
            interner: self,
            string: String::new(),
        }
    }

    pub fn intern(&mut self, str: &str) -> Intern {
        let mut builder = self.begin_intern();
        builder.push_str(str);
        builder.build()
    }
}

pub struct InternBuilder<'a> {
    interner: &'a mut Interner,
    string: String,
}

impl InternBuilder<'_> {
    pub fn push(&mut self, char: char) {
        self.string.push(char);
    }

    pub fn push_str(&mut self, str: &str) {
        self.string.push_str(str);
    }

    pub fn as_str(&self) -> &str {
        self.string.as_str()
    }

    pub fn len(&self) -> usize {
        self.string.len()
    }

    pub fn is_empty(&self) -> bool {
        self.string.is_empty()
    }

    pub fn build(self) -> Intern {
        Intern {
            text: Box::leak(self.string.into_boxed_str()),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Intern {
    text: &'static str,
}

impl Display for Intern {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.text.fmt(f)
    }
}

impl Intern {
    pub fn as_str(&self) -> &str {
        self.text
    }
}
