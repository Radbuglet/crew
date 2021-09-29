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

#[derive(Debug, Clone)] // TODO: Better debug printing
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
enum StringComponent {
    Template(TokenGroup),
    Literal(Intern),
}

#[derive(Debug, Clone)]
pub struct TokenNumberLit {
    span: Span,
    prefix: NumberPrefix,
    int: Intern,
    fp: Option<(Intern, Option<Intern>)>,
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
            prefix: None,
            digits: "0123456789",
        },
        Octal = NumberPrefixMeta {
            prefix: Some('o'),
            digits: "01234567",
        },
        Hexadecimal = NumberPrefixMeta {
            prefix: Some('x'),
            digits: "0123456789abcdef",
        },
        Binary = NumberPrefixMeta {
            prefix: Some('b'),
            digits: "01",
        },
    }

    #[derive(Debug)]
    pub enum(Option<char>) StringMode {
        Normal = None,
        Ascii = Some('a'),
        Raw = Some('r'),
        Templated = Some('$'),
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
    prefix: Option<char>,
    digits: &'static str,
}

// === Working IR === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum DelimiterMode {
    Open,
    Close,
}

#[derive(Clone)]
enum StackFrame {
    Group(TokenGroup),
    String(TokenStringLit),
    Comment,
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
    let mut reader = source.reader();

    // Consume any shebang at the beginning of the file.
    let _ = match_shebang(&mut reader);

    // Start tokenizing
    let mut stack = vec![StackFrame::Group(TokenGroup::new(
        reader.next_loc().as_span().as_owned(),
        GroupDelimiter::File,
    ))];

    loop {
        match stack.last_mut().unwrap() {
            StackFrame::Group(group) => {
                // Match group delimiters
                match group_match_delimiter(&mut reader) {
                    // Group open
                    Some((delimiter, DelimiterMode::Open)) => {
                        stack.push(StackFrame::Group(TokenGroup::new(
                            reader.next_loc().as_span().as_owned(),
                            delimiter,
                        )));
                        continue;
                    }
                    // Group close
                    Some((delimiter, DelimiterMode::Close)) => {
                        // Pop until we find a corresponding group or exits if we close the top-most
                        // delimiter, warning on mismatched delimiters. Because the EOF is a group
                        // delimiter, this code also handles unbalanced groups.
                        loop {
                            if let Some(StackFrame::Group(mut child)) = stack.pop() {
                                child.span.set_end(&reader.prev_loc());
                                match stack.last_mut() {
                                    Some(StackFrame::Group(parent)) => {
                                        parent.tokens_mut().push(Token::Group(child));

                                        // TODO: Diagnostic
                                        if parent.delimiter == delimiter {
                                            break;
                                        }
                                    }
                                    Some(StackFrame::String(_)) => todo!(),
                                    Some(StackFrame::Comment) => unreachable!(
                                        "Tokenizer was parsing groups within a comment. What?"
                                    ),
                                    None => return child,
                                }
                            } else {
                                unreachable!();
                            }
                        }

                        continue;
                    }
                    None => {}
                }

                // Match string literal start
                if let Some(token) = group_match_string_start(&mut reader) {
                    stack.push(StackFrame::String(token));
                    continue;
                }

                // Match comment start
                // We match comments before punctuation because line comments can be parsed as two
                // '/' puncts back to back and block comments can be parsed as a '/' punct followed
                // by an '*'.
                // TODO

                // Match whitespace
                if group_match_whitespace(&mut reader, false) {
                    // EOF should be unreachable here.
                    continue;
                }

                // Match identifier
                if let Some(ident) = group_match_ident(&mut reader, interner) {
                    group.tokens_mut().push(Token::Ident(ident));
                    continue;
                }

                // Match punctuation
                if let Some(punct) = group_match_punct(&mut reader) {
                    group.tokens_mut().push(Token::Punct(punct));
                    continue;
                }

                // Match number literal
                // TODO

                eprintln!(
                    // TODO
                    "Unexpected character \"{:?}\" at {}!",
                    reader.clone().consume(),
                    reader.next_loc(),
                );
            }
            StackFrame::String(_) => todo!(),
            StackFrame::Comment => todo!(),
        }
    }
}

/// Returns `true` if it was able to consume a UNIX shebang. This may match the EOF.
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

/// Matches and consumes a single group delimiter or `None` otherwise. The EOF is matched as a
/// closing [GroupDelimiter::File] delimiter.
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
pub fn group_match_ident(reader: &mut FileReader, interner: &mut Interner) -> Option<TokenIdent> {
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
        let start = reader.next_loc();

        // Read identifier start
        {
            let start = reader.consume();
            if !match_ident_start(start) {
                return None;
            }
            intern.push(start.as_char());
        }

        // Read identifier subsequent
        while reader.lookahead(|reader| {
            let subsequent = reader.consume();
            if match_ident_subsequent(subsequent) {
                intern.push(subsequent.as_char());
                true
            } else {
                false
            }
        }) {}

        // Build identifier
        Some(TokenIdent {
            span: Span::new(&start, &reader.prev_loc()),
            text: intern.build(),
        })
    })
}

/// Matches and consumes a single [TokenPunct]. Does not match the EOF.
pub fn group_match_punct(reader: &mut FileReader) -> Option<TokenPunct> {
    reader.lookahead(|reader| {
        let loc = reader.next_loc();
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

/// Returns `true` if it was able to match and consume a single whitespace character. Matches the EOF
/// is the `match_eof` flag is enabled.
pub fn group_match_whitespace(reader: &mut FileReader, match_eof: bool) -> bool {
    reader.lookahead(|reader| match reader.consume() {
        ReadAtom::Codepoint(char) => char.is_whitespace(),
        ReadAtom::Newline { .. } => true,
        ReadAtom::Unknown(_) => false,
        ReadAtom::Eof => match_eof,
    })
}

/// Matches and consumes the beginning of a [TokenStringLit].
///
/// ## Syntax
///
/// ```text
/// [prefix?: a|$] [quote: '"']
/// ```
///
/// See [StringMode] for more information about the prefixes.
///
pub fn group_match_string_start(reader: &mut FileReader) -> Option<TokenStringLit> {
    reader.lookahead(|reader| {
        let start = reader.next_loc();

        // Read string start and determine mode
        let mode = StringMode::values_iter().find_map(|(mode, prefix)| {
            // Attempt to match mode's start delimiter
            reader.lookahead(move |reader| {
                // Match prefix, if required
                if let Some(prefix) = prefix {
                    if reader.consume().as_char() != *prefix {
                        return None;
                    }
                }

                // Match quote
                if reader.consume() != ReadAtom::Codepoint('"') {
                    return None;
                }

                Some(mode)
            })
        });

        // Build literal
        mode.map(|mode| TokenStringLit {
            span: Span::new(&start, &reader.next_loc()),
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
/// ```
///
/// **Note:** we explicitly do not parse the leading sign because we handle negative numbers during
/// constant arithmetic resolution.
///
/// See [NumberPrefix] for more information about prefixes.
///
pub fn group_match_number_lit(
    _reader: &mut FileReader,
    _interner: &mut Interner,
) -> Option<TokenNumberLit> {
    todo!()
}
