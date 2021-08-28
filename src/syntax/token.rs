//! # Stage 1 - Tokenizer
//!
//! This stage accomplishes two important tasks. First, it groups the source file into balanced
//! streams, making opaque group consumption (e.g. as done by macros or un-parsable groups) much easier
//! to implement, all while providing the flexibility necessary for users to define custom syntactical
//! elements. Second, it abstracts over the complexities of Unicode to provide atomic abstractions
//! (e.g. idents, puncts, strings, comments) over them.
//!
//! In addition to the benefits of its IR, the tokenizer also provides:
//!
//! - Early interning of identifiers and literals
//! - Unbalanced group recovery
//! - Invalid character recovery
//! - "homonymous character" warnings
//! - Shebang ignoring
//!
//! TODO: Should we preserve spacing tokens so that tools like the linter can complain about them?

// === Intermediate representation === //

use crate::syntax::span::{CharOrEof, FileReader, ReadAtom, SourceFile};
use crate::util::enum_meta::{enum_meta, EnumMeta};

#[derive(Clone)]
pub enum Token {
    Group(TokenGroup),
    Ident(TokenIdent),
}

impl Token {
    pub fn debug_name(&self) -> &'static str {
        match self {
            Token::Group(_) => "group",
            Token::Ident(_) => "identifier",
        }
    }
}

#[derive(Clone)]
pub struct TokenGroup {
    tokens: Vec<Token>,
    delimiter: GroupDelimiter,
}

impl TokenGroup {
    pub fn new(delimiter: GroupDelimiter) -> Self {
        Self {
            tokens: Vec::new(),
            delimiter,
        }
    }
}

impl Into<Token> for TokenGroup {
    fn into(self) -> Token {
        Token::Group(self)
    }
}

#[derive(Clone)]
pub struct TokenIdent {
    text: String,
}

impl Into<Token> for TokenIdent {
    fn into(self) -> Token {
        Token::Ident(self)
    }
}

// === Char groups (IR) === //

enum_meta! {
    #[derive(Debug)]
    pub enum(GroupDelimiterMeta) GroupDelimiter {
        File = GroupDelimiterMeta {
            name: "file",
            left: None,
            right: CharOrEof::Eof,
        },
        Paren = GroupDelimiterMeta {
            name: "parenthesis",
            left: Some('('),
            right: CharOrEof::Char(')'),
        },
        Brace = GroupDelimiterMeta {
            name: "brace",
            left: Some('{'),
            right: CharOrEof::Char('}'),
        },
        Bracket = GroupDelimiterMeta {
            name: "square bracket",
            left: Some('['),
            right: CharOrEof::Char(']'),
        },
        Angle = GroupDelimiterMeta {
            name: "angle bracket",
            left: Some('<'),
            right: CharOrEof::Char('>'),
        },
    }
}

#[derive(Debug, Copy, Clone)]
pub struct GroupDelimiterMeta {
    name: &'static str,
    left: Option<char>,
    right: CharOrEof,
}

impl GroupDelimiter {
    pub fn resolve(left: char) -> Option<GroupDelimiter> {
        for (var, meta) in GroupDelimiter::values() {
            if Some(left) == meta.left {
                return Some(*var);
            }
        }
        None
    }
}
