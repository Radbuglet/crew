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

// === Intermediate representation === //

use crate::syntax::span::{CharOrEof, FileReader, ReadAtom, SourceFile, Span, SpanRef};
use crate::util::enum_meta::{enum_meta, EnumMeta};
use std::sync::Arc;

pub trait AnyToken {
    fn full_span(&self) -> SpanRef;
}

#[derive(Clone)]
pub enum Token {
    Group(TokenGroup),
    Ident(TokenIdent),
}

impl Token {
    pub fn as_any(&self) -> &dyn AnyToken {
        match self {
            Token::Group(group) => group as _,
            Token::Ident(ident) => ident as _,
        }
    }
}

#[derive(Clone)]
pub struct TokenGroup {
    span: Span,
    delimiter: GroupDelimiter,
    tokens: Arc<Vec<Token>>,
}

impl TokenGroup {
    pub fn new(span: Span, delimiter: GroupDelimiter) -> Self {
        Self {
            tokens: Arc::new(Vec::new()),
            span,
            delimiter,
        }
    }

    pub fn new_with(span: Span, delimiter: GroupDelimiter, tokens: Arc<Vec<Token>>) -> Self {
        Self {
            tokens,
            span,
            delimiter,
        }
    }

    pub fn delimiter(&self) -> GroupDelimiter {
        self.delimiter
    }

    pub fn set_delimiter(&mut self, delimiter: GroupDelimiter) {
        self.delimiter = delimiter;
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        Arc::make_mut(&mut self.tokens)
    }
}

impl AnyToken for TokenGroup {
    fn full_span(&self) -> SpanRef {
        self.span.as_ref()
    }
}

impl Into<Token> for TokenGroup {
    fn into(self) -> Token {
        Token::Group(self)
    }
}

#[derive(Clone)]
pub struct TokenIdent {
    span: Span,
    text: String,
}

impl Into<Token> for TokenIdent {
    fn into(self) -> Token {
        Token::Ident(self)
    }
}

impl AnyToken for TokenIdent {
    fn full_span(&self) -> SpanRef {
        self.span.as_ref()
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
            left: Some(CharOrEof::Char('(')),
            right: CharOrEof::Char(')'),
        },
        Brace = GroupDelimiterMeta {
            name: "brace",
            left: Some(CharOrEof::Char('{')),
            right: CharOrEof::Char('}'),
        },
        Bracket = GroupDelimiterMeta {
            name: "square bracket",
            left: Some(CharOrEof::Char('[')),
            right: CharOrEof::Char(']'),
        },
        Angle = GroupDelimiterMeta {
            name: "angle bracket",
            left: Some(CharOrEof::Char('<')),
            right: CharOrEof::Char('>'),
        },
    }
}

#[derive(Debug, Copy, Clone)]
pub struct GroupDelimiterMeta {
    name: &'static str,
    left: Option<CharOrEof>,
    right: CharOrEof,
}

// === Tokenizing === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum DelimiterMode {
    Open,
    Close,
}

enum FlatToken {
    Delimiter(DelimiterMode, GroupDelimiter),
    Ident(TokenIdent),
}

// TODO: Error reporting mechanisms

pub fn tokenize_span(source: &SourceFile) -> TokenGroup {
    let mut reader = source.reader();

    // Consume shebang
    consume_shebang(&mut reader);

    // Parse flat token stream
    let mut tokens = Vec::new();
    loop {
        // Parse whitespace
        if consume_whitespace(&mut reader) {
            continue;
        }

        // Parse group delimiters
        if let Some((mode, delimiter)) = consume_delimiter(&mut reader) {
            tokens.push(FlatToken::Delimiter(mode, delimiter));
            continue;
        }

        // Parse identifiers
        // TODO
    }

    // Transform flat token stream into grouped stream
    // TODO
}

fn consume_shebang(reader: &mut FileReader) {
    let mut lookahead = reader.clone();

    // Magic "number"
    if lookahead.consume() != ReadAtom::Codepoint('#') {
        return;
    }

    if lookahead.consume() != ReadAtom::Codepoint('!') {
        return;
    }

    // Read until line end
    loop {
        let spacing = lookahead.consume();
        // We don't warn on illegal characters since we're not the one parsing this section.
        if spacing.is_newline() || spacing == ReadAtom::Eof {
            break;
        }
    }

    // Commit
    *reader = lookahead;
}

fn consume_seq(reader: &mut FileReader, seq: &[CharOrEof]) -> bool {
    reader.lookahead(|reader| {
        for pattern_char in seq {
            if reader.consume().as_char() != *pattern_char {
                return false;
            }
        }
        true
    })
}

fn consume_delimiter(reader: &mut FileReader) -> Option<(DelimiterMode, GroupDelimiter)> {
    reader.lookahead(|reader| {
        let char = reader.consume().as_char();
        for (delimiter, meta) in GroupDelimiter::values() {
            if meta.left == Some(char) {
                return Some((DelimiterMode::Open, *delimiter));
            }

            if meta.right == char {
                return Some((DelimiterMode::Close, *delimiter));
            }
        }
        None
    })
}

fn consume_whitespace(reader: &mut FileReader) -> bool {
    reader.lookahead(|reader| {
        let atom = reader.consume();
        atom.is_newline() || atom.as_char().to_char().is_whitespace()
    })
}
