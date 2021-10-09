//! # Stage 1 - Tokenizer
//!
//! This module contains both the token tree IR and the producer logic for the compiler's
//! tokenization stage.
//!
//! This stage accomplishes two important tasks. First, it groups the source file into balanced
//! token streams--making opaque group consumption (e.g. as done by macros or un-parsable groups) much
//! easier to implement--while providing the flexibility necessary for users to define custom
//! syntactical elements. Second, it abstracts over the complexities of Unicode by providing atomic
//! abstractions (e.g. idents, puncts, strings, comments) over them.
//!
//! In addition to the benefits of its IR, the tokenizer also handles:
//!
//! - Early interning of identifiers and literals
//! - Unbalanced group recovery
//! - Invalid character recovery
//! - Literal parsing
//! - Shebang ignoring
//!
//! We merged the lexing and tokenization stages into one tokenization stage because the way characters
//! are lexed depends upon the token's hierarchical context. The necessity of this context is most
//! apparent in templated strings because strings and token groups can be nested in one another,
//! preventing us from distinguishing between code-significant lexemes and the more liberal string
//! literal character-set without knowing which token we're currently in.
//!
//! The closest things to a lexer present in this module are the `mode_match_xx` functions. These
//! attempt to consume "logical lexemes" but enable the user to select which lexemes can be matched
//! given the specific context of the main [tokenize_file] routine.

use crate::syntax::span::{
    AsFileReader, CharOrEof, FileLoc, FileLocRef, FileReader, ReadAtom, Span, SpanRef,
};
use crate::util::enum_meta::{enum_meta, EnumMeta};
use crate::util::reader::LookaheadReader;
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::iter::FromIterator;
use std::sync::Arc;

// === IR === //

pub trait AnyToken: Display {
    fn full_span(&self) -> SpanRef;
}

#[derive(Debug, Default, Clone)]
pub struct TokenStream {
    tokens: Arc<Vec<Token>>,
}

impl TokenStream {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with(tokens: Vec<Token>) -> Self {
        Self {
            tokens: Arc::new(tokens),
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        Arc::make_mut(&mut self.tokens)
    }
}

impl FromIterator<Token> for TokenStream {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        Self::new_with(iter.into_iter().collect())
    }
}

impl<'a> IntoIterator for &'a TokenStream {
    type Item = &'a Token;
    type IntoIter = std::slice::Iter<'a, Token>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens().iter()
    }
}

#[derive(Debug, Clone)]
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
    pub span: Span,
    pub delimiter: GroupDelimiter,
    pub stream: TokenStream,
}

impl TokenGroup {
    pub fn new(span: Span, delimiter: GroupDelimiter) -> Self {
        Self {
            stream: TokenStream::new(),
            span,
            delimiter,
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        self.stream.tokens()
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        self.stream.tokens_mut()
    }

    pub fn opening_brace(&self) -> FileLocRef {
        self.span.start()
    }

    pub fn closing_brace(&self) -> FileLocRef {
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
    pub span: Span,
    pub text: String,
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
    pub loc: FileLoc,
    pub is_glued: bool,
    pub punct: PunctChar,
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
    pub span: Span,
    pub mode: StringMode,
    pub parts: Vec<StringComponent>,
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
pub enum StringComponent {
    Template(TokenGroup),
    Literal(String),
}

#[derive(Debug, Clone)]
pub struct TokenNumberLit {
    pub span: Span,
    pub prefix: NumberPrefix,
    pub int_part: String,
    pub float_part: Option<String>,
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

pub const DIGITS_DECIMAL: &'static str = "0123456789";
pub const DIGITS_HEXADECIMAL: &'static str = "0123456789abcdef";

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
        Le = '<',
        Ge = '>',
    }

    #[derive(Debug)]
    pub enum(NumberPrefixMeta) NumberPrefix {
        Decimal = NumberPrefixMeta {
            prefix: None,
            digits: DIGITS_DECIMAL,
        },
        Octal = NumberPrefixMeta {
            prefix: Some('o'),
            digits: "01234567",
        },
        Hexadecimal = NumberPrefixMeta {
            prefix: Some('x'),
            digits: DIGITS_HEXADECIMAL,
        },
        Binary = NumberPrefixMeta {
            prefix: Some('b'),
            digits: "01",
        },
    }

    #[derive(Debug)]
    pub enum(Option<char>) StringMode {
        Normal = None,
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

// === IR printing === //

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        self.as_any().fmt(f)
    }
}

impl Display for TokenStream {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        for token in self.tokens() {
            if !first {
                write!(f, " ")?;
            }
            token.fmt(f)?;
            first = false;
        }
        Ok(())
    }
}

impl Display for TokenGroup {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let delimiter = self.delimiter.meta();

        match (delimiter.left, delimiter.right) {
            (Some(CharOrEof::Char(start)), CharOrEof::Char(end)) => {
                write!(f, "{} ", start)?;
                self.stream.fmt(f)?;
                write!(f, " {}", end)?;
                Ok(())
            }
            (None, CharOrEof::Eof) => self.stream.fmt(f),
            _ => unreachable!(),
        }
    }
}

impl Display for TokenIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.text)
    }
}

impl Display for TokenPunct {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.punct.meta())
    }
}

impl Display for TokenStringLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(prefix_char) = self.mode.meta() {
            write!(f, "{}", prefix_char)?;
        }
        write!(f, "\"")?;
        for comp in &self.parts {
            comp.fmt(f)?;
        }
        write!(f, "\"")?;
        Ok(())
    }
}

impl Display for TokenNumberLit {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        if let Some(prefix) = self.prefix.meta().prefix {
            write!(f, "0{}", prefix)?;
        }
        write!(f, "{}", self.int_part)?;
        if let Some(fp) = &self.float_part {
            write!(f, ".{}", fp)?;
        }

        Ok(())
    }
}

impl Display for StringComponent {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            StringComponent::Literal(literal) => literal.fmt(f),
            StringComponent::Template(group) => group.fmt(f),
        }
    }
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
    BlockComment,
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

pub fn tokenize_file<R: ?Sized + AsFileReader>(source: &R) -> TokenGroup {
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
                        assert_eq!(
                            delimiter,
                            group.delimiter,
                            "Delimiter balance error between brace at {} and brace at {}",
                            group.full_span().start().pos(),
                            reader.prev_loc().pos(),
                        );

                        let child = match stack.pop() {
                            Some(StackFrame::Group(group)) => group,
                            _ => unreachable!(),
                        };

                        match stack.last_mut() {
                            Some(StackFrame::Group(parent)) => {
                                parent.tokens_mut().push(child.into())
                            }
                            Some(StackFrame::String(str)) => {
                                str.parts.push(StringComponent::Template(child))
                            }
                            Some(StackFrame::BlockComment) => unreachable!(),
                            None => return child,
                        }

                        continue;
                    }
                    None => {}
                }

                // Sanity check
                debug_assert_ne!(reader.peek(), ReadAtom::Eof);

                // Match string literal start
                if let Some(token) = group_match_string_start(&mut reader) {
                    stack.push(StackFrame::String(token));
                    continue;
                }

                // Match comment start
                // We match comments before punctuation because line comments can be parsed as two
                // '/' puncts back to back and block comments can be parsed as a '/' punct followed
                // by an '*'.
                if group_match_line_comment(&mut reader) {
                    continue;
                }

                if group_match_block_comment_start(&mut reader) {
                    stack.push(StackFrame::BlockComment);
                    continue;
                }

                // Match whitespace
                if group_match_whitespace(&mut reader, false) {
                    continue;
                }

                // Match identifier
                if let Some(ident) = group_match_ident(&mut reader) {
                    group.tokens_mut().push(Token::Ident(ident));
                    continue;
                }

                // Match punctuation
                {
                    let glued_punct = match group.tokens().last() {
                        Some(Token::Punct(_)) => true,
                        _ => false,
                    };

                    if let Some(punct) = group_match_punct(&mut reader, glued_punct) {
                        group.tokens_mut().push(Token::Punct(punct));
                        continue;
                    }
                }

                // Match number literal
                if let Some(num) = group_match_number_lit(&mut reader) {
                    if group_match_ident(&mut reader.clone()).is_some() {
                        panic!("Unexpected digit at {}", reader.next_loc().pos());
                    }

                    group.tokens_mut().push(Token::NumberLit(num));
                    continue;
                }

                panic!(
                    "Unexpected character in group at {}!",
                    reader.next_loc().pos()
                );
            }
            StackFrame::String(string) => {
                let mut text = String::new();
                loop {
                    // Match EOF
                    if reader.peek() == ReadAtom::Eof {
                        panic!(
                            "Unexpected EOF while parsing string starting at {}",
                            string.full_span().start().pos()
                        );
                    }

                    // Match multiline escape
                    // We match multiline escapes before other character escapes because the latter
                    // does not recognize the newline escape.
                    if string_match_multiline_escape(&mut reader) {
                        continue;
                    }

                    // We match escape sequences before any special delimiter characters because the
                    // escape might be for a delimiter.
                    if let Some(char) = string_match_escape(&mut reader) {
                        text.push(char);
                        continue;
                    }

                    // Match closing quote
                    if string_match_end(&mut reader) {
                        if !text.is_empty() {
                            string.parts.push(StringComponent::Literal(text));
                        }

                        let string = match stack.pop() {
                            Some(StackFrame::String(string)) => string,
                            _ => unreachable!(),
                        };

                        match stack.last_mut() {
                            Some(StackFrame::Group(group)) => {
                                group.tokens_mut().push(string.into())
                            }
                            Some(StackFrame::String(_)) => unreachable!(),
                            Some(StackFrame::BlockComment) => unreachable!(),
                            None => unreachable!(),
                        }

                        break;
                    }

                    // Match placeholder
                    if string.mode == StringMode::Templated
                        && string_match_placeholder_start(&mut reader)
                    {
                        if !text.is_empty() {
                            string.parts.push(StringComponent::Literal(text));
                        }

                        stack.push(
                            TokenGroup::new(
                                Span::new(&reader.prev_loc(), &reader.prev_loc()),
                                GroupDelimiter::Brace,
                            )
                            .into(),
                        );

                        break;
                    }

                    // Match character
                    if let Some(char) = string_match_char(&mut reader) {
                        text.push(char);
                        continue;
                    }

                    panic!(
                        "Unexpected character in string at {}!",
                        reader.next_loc().pos()
                    );
                }
            }
            StackFrame::BlockComment => {
                if group_match_block_comment_start(&mut reader) {
                    stack.push(StackFrame::BlockComment);
                    continue;
                }

                if comment_match_block_end(&mut reader) {
                    stack.pop();
                    continue;
                }

                if reader.peek() == ReadAtom::Eof {
                    panic!("Unexpected EOF while parsing block comment!");
                }

                // Yes, this ignores invalid unicode characters. Comments really are the wild west.
                let _ = reader.consume();
            }
        }
    }
}

pub fn match_str(reader: &mut FileReader, text: &str) -> bool {
    reader.lookahead(|reader| {
        text.chars()
            .all(|expected| reader.consume().as_char() == expected)
    })
}

/// Returns `true` if it was able to consume a UNIX shebang. Does not match a plain EOF.
pub fn match_shebang(reader: &mut FileReader) -> bool {
    reader.lookahead(|reader| {
        // Magic "number"
        if !match_str(reader, "#!") {
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

/// Matches and consumes a [TokenIdent] that is at least one character long. Does not match a plain EOF.
pub fn group_match_ident(reader: &mut FileReader) -> Option<TokenIdent> {
    reader.lookahead(|reader| {
        let mut text = String::new();
        let start = reader.next_loc();

        // Match identifier start
        if !reader.lookahead(|reader| {
            let char = reader.consume().as_char();
            if char.is_alphabetic() || char == '_' {
                text.push(char);
                true
            } else {
                false
            }
        }) {
            return None;
        }

        // Match identifier subsequent
        reader.consume_while(|reader| {
            let char = reader.consume().as_char();
            if char.is_alphanumeric() || char == '_' {
                text.push(char);
                true
            } else {
                false
            }
        });

        // Build identifier
        Some(TokenIdent {
            span: Span::new(&start, &reader.prev_loc()),
            text,
        })
    })
}

/// Matches and consumes a single [TokenPunct]. Does not match a plain EOF.
pub fn group_match_punct(reader: &mut FileReader, is_glued: bool) -> Option<TokenPunct> {
    reader.lookahead(|reader| {
        let start = reader.next_loc();
        let read = match reader.consume() {
            ReadAtom::Codepoint(char) => char,
            _ => return None,
        };

        PunctChar::values_iter().find_map(move |(punct, char)| {
            if read == *char {
                Some(TokenPunct {
                    loc: start.as_owned(),
                    is_glued,
                    punct,
                })
            } else {
                None
            }
        })
    })
}

/// Returns `true` if it was able to match and consume a single whitespace character. Can match an
/// empty EOF if the `match_eof` flag is enabled.
pub fn group_match_whitespace(reader: &mut FileReader, match_eof: bool) -> bool {
    reader.lookahead(|reader| match reader.consume() {
        ReadAtom::Codepoint(char) => char.is_whitespace(),
        ReadAtom::Newline(_) => true,
        ReadAtom::Unknown(_) => false,
        ReadAtom::Eof => match_eof,
    })
}

/// Matches and consumes the beginning of a [TokenStringLit]. Does not match a plain EOF.
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
            parts: Vec::new(),
        })
    })
}

/// Matches and consumes a single [TokenNumberLit]. Does not match a plain EOF.
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
pub fn group_match_number_lit(reader: &mut FileReader) -> Option<TokenNumberLit> {
    fn read_digits(reader: &mut FileReader, digits: &str) -> String {
        fn alphabet_contains(alphabet: &str, char: char) -> bool {
            let char = char.to_ascii_lowercase();
            alphabet.chars().any(|allowed| allowed == char)
        }

        let mut text = String::new();
        reader.consume_while(|reader| match reader.consume() {
            ReadAtom::Codepoint(char) if alphabet_contains(digits, char) => {
                text.push(char.to_ascii_lowercase());
                true
            }
            ReadAtom::Codepoint('_') => true,
            _ => false,
        });
        text
    }

    reader.lookahead(|reader| {
        let start = reader.next_loc();

        // Attempt to match prefix
        let prefix = reader
            .lookahead(|reader| {
                // Consume '0'
                if reader.consume() != ReadAtom::Codepoint('0') {
                    return None;
                }

                // Consume prefix character
                let read = reader.consume().as_char().to_ascii_lowercase();
                NumberPrefix::values_iter().find_map(move |(prefix, meta)| {
                    if Some(read) == meta.prefix {
                        Some(prefix)
                    } else {
                        None
                    }
                })
            })
            .unwrap_or(NumberPrefix::Decimal);

        // Read integer-part digits
        let int_part = {
            let text = read_digits(reader, prefix.meta().digits);
            if text.is_empty() {
                if prefix == NumberPrefix::Decimal {
                    return None;
                } else {
                    panic!("Expected digits after prefix at {}!", start.pos());
                }
            }
            text
        };

        // Read floating-part digits
        let float_part = {
            // Match period
            let has_fp = match_str(reader, ".");

            if has_fp {
                if prefix != NumberPrefix::Decimal {
                    panic!(
                        "Cannot specify a floating part for non-decimal numbers at {}.",
                        reader.prev_loc().pos()
                    );
                }

                Some(read_digits(reader, &NumberPrefix::Decimal.meta().digits))
            } else {
                None
            }
        };

        Some(TokenNumberLit {
            prefix,
            span: Span::new(&start, &reader.prev_loc().as_owned()),
            int_part,
            float_part,
        })
    })
}

pub fn group_match_line_comment(reader: &mut FileReader) -> bool {
    reader.lookahead(|reader| {
        if !match_str(reader, "//") {
            return false;
        }

        reader.consume_while(|reader| {
            let char = reader.consume();
            !char.is_newline_like() && char != ReadAtom::Eof
        });

        true
    })
}

pub fn group_match_block_comment_start(reader: &mut FileReader) -> bool {
    match_str(reader, "/*")
}

pub fn string_match_multiline_escape(reader: &mut FileReader) -> bool {
    if match_str(reader, "\\\n") {
        reader.consume_while(|reader| group_match_whitespace(reader, false));
        true
    } else {
        false
    }
}

pub fn string_match_escape(reader: &mut FileReader) -> Option<char> {
    reader.lookahead(|reader| {
        if match_str(reader, "\\") {
            match reader.consume() {
                // ASCII characters
                ReadAtom::Codepoint('r') => Some('\r'),
                ReadAtom::Codepoint('n') => Some('\n'),
                ReadAtom::Codepoint('t') => Some('\t'),
                ReadAtom::Codepoint('0') => Some('\0'),

                // Delimiter escapes
                ReadAtom::Codepoint('\\') => Some('\\'),
                ReadAtom::Codepoint('"') => Some('"'),
                ReadAtom::Codepoint('\'') => Some('\''),
                ReadAtom::Codepoint('{') => Some('{'),

                // Unicode
                ReadAtom::Codepoint('u') => todo!("Unicode escapes not supported yet!"),
                _ => panic!(
                    "Invalid character escape sequence at {}!",
                    reader.prev_loc().pos()
                ),
            }
        } else {
            None
        }
    })
}

pub fn string_match_char(reader: &mut FileReader) -> Option<char> {
    if let Some(char) = reader.lookahead(|reader| match reader.consume() {
        ReadAtom::Codepoint(char) => Some(char),
        ReadAtom::Newline(_) => Some('\n'),
        _ => None,
    }) {
        return Some(char);
    }

    None
}

pub fn string_match_placeholder_start(reader: &mut FileReader) -> bool {
    match_str(reader, "{")
}

pub fn string_match_end(reader: &mut FileReader) -> bool {
    match_str(reader, "\"")
}

pub fn comment_match_block_end(reader: &mut FileReader) -> bool {
    match_str(reader, "*/")
}
