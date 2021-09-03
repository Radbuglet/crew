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

use crate::syntax::span::{AsFileReader, CharOrEof, FileLoc, FileReader, ReadAtom, Span, SpanRef};
use crate::util::enum_meta::{enum_meta, EnumMeta};
use std::sync::Arc;

pub trait AnyToken {
    fn full_span(&self) -> SpanRef;
}

#[derive(Clone)]
pub enum Token {
    Group(TokenGroup),
    Ident(TokenIdent),
    Punct(TokenPunct),
}

impl Token {
    pub fn as_any(&self) -> &dyn AnyToken {
        match self {
            Token::Group(group) => group as _,
            Token::Ident(ident) => ident as _,
            Token::Punct(punct) => punct as _,
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct TokenPunct {
    loc: FileLoc,
    punct: PunctChar,
}

impl Into<Token> for TokenPunct {
    fn into(self) -> Token {
        Token::Punct(self)
    }
}

impl AnyToken for TokenPunct {
    fn full_span(&self) -> SpanRef {
        SpanRef::new_ref(&self.loc, &self.loc)
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

    #[derive(Debug)]
    pub enum(char) PunctChar {
        Backtick = '`',
        Tilde = '~',
        Exclamation = '!',
        At = '@',
        Pound = '#',
        Dollar = '$',
        Percent = '%',
        Caret = '^',
        Ampersand = '&',
        Asterisk = '*',
        Dash = '-',
        Plus = '+',
        Equals = '=',
        Bar = '|',
        Semicolon = ';',
        Colon = ':',
        Comma = ',',
        Period = '.',
        Question = '?',
        Slash = '/',
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

#[derive(Debug)]
enum FlatToken {
    Delimiter(DelimiterMode, GroupDelimiter),
    Ident(TokenIdent),
    Punct(TokenPunct),
}

// TODO: Error reporting mechanisms

pub fn tokenize_file<R: ?Sized + AsFileReader>(source: &R) -> TokenGroup {
    let mut reader = source.reader();

    // Consume shebang
    consume_shebang(&mut reader);

    // Parse flat token stream
    let mut tokens = Vec::new();
    loop {
        // End on EOF
        if reader.peek().is_eof() {
            break;
        }

        // Parse whitespace
        if consume_whitespace(&mut reader) {
            continue;
        }

        // Parse group delimiters (open and close)
        if let Some((mode, delimiter)) = consume_delimiter(&mut reader) {
            tokens.push(FlatToken::Delimiter(mode, delimiter));
            continue;
        }

        // Parse identifiers
        if let Some(ident) = consume_ident(&mut reader) {
            tokens.push(FlatToken::Ident(TokenIdent {
                span: ident.as_owned(),
                text: ident.as_str().to_string(),
            }));
            continue;
        }

        // Parse punctuation
        if let Some(punct) = consume_punct(&mut reader) {
            tokens.push(FlatToken::Punct(TokenPunct {
                loc: reader.loc().as_owned(),
                punct,
            }));
            continue;
        }

        // Parse literals
        // TODO

        // Parse comments
        // TODO

        // Handle unexpected character
        let unexpected = reader.consume();
        eprintln!("Unexpected: {}", unexpected.as_char_or_eof().nul_eof());
    }

    println!("Tokens: {:#?}", tokens);

    // Transform flat token stream into grouped stream
    todo!()
}

// === Tokenize components === //

fn consume_shebang(reader: &mut FileReader) {
    reader.lookahead(|reader| {
        // Magic "number"
        if reader.consume() != ReadAtom::Codepoint('#') {
            return false;
        }

        if reader.consume() != ReadAtom::Codepoint('!') {
            return false;
        }

        // Read until line end
        loop {
            let spacing = reader.consume();
            // We don't warn on illegal characters since we're not the one parsing this section.
            if spacing.is_newline() || spacing == ReadAtom::Eof {
                break;
            }
        }

        true
    });
}

fn consume_delimiter(reader: &mut FileReader) -> Option<(DelimiterMode, GroupDelimiter)> {
    reader.lookahead(|reader| {
        let char = reader.consume().as_char_or_eof();
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

fn consume_punct(reader: &mut FileReader) -> Option<PunctChar> {
    reader.lookahead(|reader| {
        let char = reader.consume().as_char_or_eof().nul_eof();
        for (punct, match_char) in PunctChar::values().iter().copied() {
            if char == match_char {
                return Some(punct);
            }
        }
        None
    })
}

fn consume_whitespace(reader: &mut FileReader) -> bool {
    reader.lookahead(|reader| is_whitespace(reader.consume()))
}

fn consume_ident(reader: &mut FileReader) -> Option<Span> {
    fn is_ident_start(atom: ReadAtom) -> bool {
        match atom {
            ReadAtom::Codepoint(char) => char.is_alphabetic() || char == '_',
            _ => false,
        }
    }

    fn is_ident_subsequent(atom: ReadAtom) -> bool {
        match atom {
            ReadAtom::Codepoint(char) => char.is_alphanumeric() || char == '_',
            _ => false,
        }
    }

    reader.lookahead(|reader| {
        let start = reader.loc();

        // Consume first ident char
        if !is_ident_start(reader.consume()) {
            return None;
        }

        // Read until first non-ident char
        let mut end = start;
        while is_ident_subsequent(reader.peek()) {
            end = reader.loc();
            reader.consume();
        }

        // Construct ident span
        Some(Span::new_owned(&start, &end))
    })
}

fn is_whitespace(atom: ReadAtom) -> bool {
    atom.as_char_or_eof().nul_eof().is_whitespace()
}

fn consume_str_seq(reader: &mut FileReader, seq: &[CharOrEof]) -> bool {
    reader.lookahead(|reader| {
        for pattern_char in seq {
            if reader.consume().as_char_or_eof() != *pattern_char {
                return false;
            }
        }
        true
    })
}
