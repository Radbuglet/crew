//! # Stage 1 - Tokenizer
//!
//! This stage accomplishes two important tasks. First, it groups the source file into balanced
//! streams--making opaque group consumption (e.g. as done by macros or un-parsable groups) much easier
//! to implement--while providing the flexibility necessary for users to define custom syntactical
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

use crate::syntax::intern::{Intern, Interner};
use crate::syntax::span::{
    AsFileReader, CharOrEof, FileLoc, FileLocRef, FileReader, ReadAtom, Span, SpanRef,
};
use crate::util::enum_meta::{enum_meta, EnumMeta};
use smallvec::SmallVec;
use std::sync::Arc;

// === Intermediate representation === //

pub trait AnyToken {
    fn full_span(&self) -> SpanRef;
}

#[derive(Clone)]
pub enum Token {
    Group(TokenGroup),
    Ident(TokenIdent),
    Punct(TokenPunct),
    StringLit(TokenStringLit),
    NumberLit(TokenNumberLit),
}

impl Token {
    pub fn as_any(&self) -> &dyn AnyToken {
        match self {
            Token::Group(group) => group as _,
            Token::Ident(ident) => ident as _,
            Token::Punct(punct) => punct as _,
            Token::StringLit(string) => string as _,
            Token::NumberLit(number) => number as _,
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

    pub fn beginning(&self) -> FileLocRef {
        self.span.start()
    }

    pub fn end(&self) -> FileLocRef {
        self.span.end()
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
    text: Intern,
}

impl AnyToken for TokenIdent {
    fn full_span(&self) -> SpanRef {
        self.span.as_ref()
    }
}

impl Into<Token> for TokenIdent {
    fn into(self) -> Token {
        Token::Ident(self)
    }
}

#[derive(Clone)]
pub struct TokenPunct {
    loc: FileLoc,
    punct: PunctChar,
}

impl AnyToken for TokenPunct {
    fn full_span(&self) -> SpanRef {
        SpanRef::new(&self.loc, &self.loc)
    }
}

impl Into<Token> for TokenPunct {
    fn into(self) -> Token {
        Token::Punct(self)
    }
}

#[derive(Clone)]
pub struct TokenStringLit {
    span: Span,
    mode: StringMode,
    parts: SmallVec<[StringComponent; 1]>,
}

impl AnyToken for TokenStringLit {
    fn full_span(&self) -> SpanRef {
        self.span.as_ref()
    }
}

impl Into<Token> for TokenStringLit {
    fn into(self) -> Token {
        Token::StringLit(self)
    }
}

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
pub enum StringMode {
    Normal,
    Ascii,
    Templated,
}

#[derive(Clone)]
enum StringComponent {
    Template(TokenGroup),
    Literal(Intern),
}

#[derive(Clone)]
pub struct TokenNumberLit {
    span: Span,
    prefix: NumberPrefix,
    int: Intern,
    fp: Option<(Intern, Option<Intern>)>,
    suffix: Option<NumberSuffix>,
}

impl AnyToken for TokenNumberLit {
    fn full_span(&self) -> SpanRef {
        self.span.as_ref()
    }
}

impl Into<Token> for TokenNumberLit {
    fn into(self) -> Token {
        Token::NumberLit(self)
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

    #[derive(Debug)]
    pub enum(NumberPrefixMeta) NumberPrefix {
        Decimal = NumberPrefixMeta {
            prefix_char: None,
            int_digits: "0123456789",
        },
        Octal = NumberPrefixMeta {
            prefix_char: Some('o'),
            int_digits: "01234567",
        },
        Hexadecimal = NumberPrefixMeta {
            prefix_char: Some('x'),
            int_digits: "0123456789abcdef",
        },
        Binary = NumberPrefixMeta {
            prefix_char: Some('b'),
            int_digits: "01",
        },
    }

    #[derive(Debug)]
    pub enum(&'static str) NumberSuffix {
        U8 = "u8",
        U16 = "u16",
        U32 = "u32",
        U64 = "u64",
        I8 = "u8",
        I16 = "u16",
        I32 = "u32",
        I64 = "u64",
        F32 = "f32",
        F64 = "f64",
    }
}

#[derive(Debug, Copy, Clone)]
pub struct GroupDelimiterMeta {
    name: &'static str,
    left: Option<CharOrEof>,
    right: CharOrEof,
}

#[derive(Debug, Copy, Clone)]
pub struct NumberPrefixMeta {
    prefix_char: Option<char>,
    int_digits: &'static str,
}

// === Misc IR === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum DelimiterMode {
    Open,
    Close,
}

#[derive(Clone)]
enum StackFrame {
    Group(TokenGroup),
    String(TokenStringLit),
}

impl Into<StackFrame> for TokenGroup {
    fn into(self) -> StackFrame {
        StackFrame::Group(self)
    }
}

impl Into<StackFrame> for TokenStringLit {
    fn into(self) -> StackFrame {
        StackFrame::String(self)
    }
}

// === Tokenizing === //

// TODO: Error reporting mechanisms

pub fn tokenize_file<R: ?Sized + AsFileReader>(interner: &mut Interner, source: &R) -> TokenGroup {
    todo!()
}

/// Returns `true` if it was able to consume a UNIX shebang, leaving the cursor one character past
/// the newline or at the EOF. This may match the EOF.
pub fn match_shebang(reader: &mut FileReader) -> bool {
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
            // We treat a carriage return as a newline because we're more likely to encounter a
            // malformed newline than a shebang which includes a CR in the command name/arguments.
            if spacing.is_newline_like() || spacing == ReadAtom::Eof {
                break;
            }
        }

        true
    })
}

/// Matches and consumes a single group delimiter or `None` otherwise. This may match the EOF.
pub fn group_match_delimiter(reader: &mut FileReader) -> Option<(GroupDelimiter, DelimiterMode)> {
    reader.lookahead(|reader| {
        let read = reader.consume().as_char_or_eof();
        GroupDelimiter::values_iter().find_map(move |(delimiter, meta)| {
            if meta.left == Some(read) {
                Some((delimiter, DelimiterMode::Open))
            } else if meta.right == read {
                Some((delimiter, DelimiterMode::Close))
            } else {
                None
            }
        })
    })
}

/// Matches and consumes a [TokenIdent] that is at least one character long. Does not match the EOF.
pub fn group_match_ident(interner: &mut Interner, reader: &mut FileReader) -> Option<TokenIdent> {
    fn match_ident_start(atom: ReadAtom) -> bool {
        let char = atom.as_char();
        char.is_alphabetic() || char == '_'
    }

    fn match_ident_subsequent(atom: ReadAtom) -> bool {
        let char = atom.as_char();
        char.is_alphanumeric() || char == '_'
    }

    reader.lookahead(|reader| {
        let mut intern = interner.begin_intern();
        let start = reader.loc();
        let mut end = reader.loc();

        // Read identifier start
        if !match_ident_start(reader.peek()) {
            return None;
        }
        intern.push(reader.consume().as_char());

        // Read identifier subsequent
        while match_ident_subsequent(reader.peek()) {
            end = reader.loc();
            intern.push(reader.peek().as_char());
            reader.consume();
        }

        // Build identifier
        Some(TokenIdent {
            span: SpanRef::new(&start, &end).as_owned(),
            text: intern.build(),
        })
    })
}

/// Matches and consumes a single [TokenPunct]. Does not match the EOF.
pub fn group_match_punct(reader: &mut FileReader) -> Option<TokenPunct> {
    reader.lookahead(|reader| {
        let loc = reader.loc();
        let read = match reader.consume() {
            ReadAtom::Codepoint(char) => char,
            _ => return None,
        };

        PunctChar::values_iter().find_map(move |(punct, char)| {
            if read == *char {
                Some(TokenPunct {
                    loc: loc.as_owned(),
                    punct,
                })
            } else {
                None
            }
        })
    })
}

/// Returns `true` if it was able to match and consume a single whitespace character. Does not match
/// the EOF.
pub fn group_match_whitespace(reader: &mut FileReader) -> bool {
    reader.lookahead(|reader| match reader.consume() {
        ReadAtom::Codepoint(char) => char.is_whitespace(),
        ReadAtom::Newline { .. } => true,
        ReadAtom::Unknown(_) => false,
        ReadAtom::Eof => false,
    })
}

/// Matches and consumes the beginning of a [TokenStringLit].
///
/// ## Syntax
///
/// ```text
/// [prefix?: a|$] [quote: "]
/// ```
///
/// See [StringMode] for more information about the prefixes.
///
pub fn group_match_string_start(reader: &mut FileReader) -> Option<TokenStringLit> {
    reader.lookahead(|reader| {
        let start = reader.loc();
        let mut end = reader.loc();

        // Read string start and determine mode
        let mode = match reader.consume() {
            // TODO: Binary string mode, use prefix consuming system.
            ReadAtom::Codepoint('"') => StringMode::Normal,
            ReadAtom::Codepoint('a') => {
                end = reader.loc();
                if let ReadAtom::Codepoint('"') = reader.consume() {
                    StringMode::Ascii
                } else {
                    return None;
                }
            }
            ReadAtom::Codepoint('$') => {
                end = reader.loc();
                if let ReadAtom::Codepoint('"') = reader.consume() {
                    StringMode::Templated
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        // Build literal
        Some(TokenStringLit {
            span: SpanRef::new(&start, &end).as_owned(),
            mode,
            parts: SmallVec::new(),
        })
    })
}

/// Matches and consumes a single [TokenNumberLit]. Does not match the EOF.
///
/// ## Syntax
///
/// ```text
/// [prefix?: "0" + (x|o|b)]
/// [int_part: PREFIX_DIGITS]
/// [decimal (only when FP)?:
///     "." + DEC_DIGIT*
///     [exponent?: "E" + (+|-) + DEC_DIGIT*]
/// ]
/// [type?: TYPE_SUFFIXES]
/// ```
///
/// See [NumberPrefix] for more information about prefixes.
///
/// See [NumberSuffix] for more information about type suffixes.
///
pub fn group_match_number_lit(
    reader: &mut FileReader,
    interner: &mut Interner,
) -> Option<TokenNumberLit> {
    reader.lookahead(|reader| {
        let start = reader.loc();
        let mut end = reader.loc();

        // Attempt to match prefix
        let prefix = reader
            .lookahead(|reader| {
                // Consume '0'
                if reader.consume().as_char() != '0' {
                    return None;
                }

                // Consume prefix character
                let read = reader.consume().as_char().to_ascii_lowercase();
                NumberPrefix::values_iter().find_map(move |(prefix, meta)| {
                    if Some(read) == meta.prefix_char {
                        Some(prefix)
                    } else {
                        None
                    }
                })
            })
            .unwrap_or(NumberPrefix::Decimal);

        // Read integer-part digits
        let mut int_part = interner.begin_intern();
        let is_valid_char = move |char: char| {
            let char = char.to_ascii_lowercase();
            prefix
                .meta()
                .int_digits
                .chars()
                .any(|allowed| allowed == char)
        };

        loop {
            let matched = reader.lookahead(|reader| match reader.consume() {
                ReadAtom::Codepoint(char) if is_valid_char(char) => {
                    Some(Some(char.to_ascii_lowercase()))
                }
                ReadAtom::Codepoint('_') => Some(None),
                ReadAtom::Newline { .. } => None,
                ReadAtom::Eof => None,
                _ => todo!(),
            });

            if let Some(char) = matched {
                if let Some(char) = char {
                    int_part.push(char);
                }
                end = reader.loc();
            } else {
                break;
            }
        }
        let int_part = int_part.build();

        // TODO: Finish

        println!("Parsed: {:?} {}", prefix, interner.resolve(int_part));

        None
    })
}
