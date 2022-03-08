use crate::syntax::span::{CharOrEof, FileLoc, Span};
use crate::util::enum_utils::{enum_categories, enum_meta, EnumMeta};
use crate::util::reader::{LookaheadReader, StreamReader};
use std::fmt::{Display, Formatter, Result as FmtResult};
use std::iter::FromIterator;
use std::rc::Rc;

// === Token Stream === //

#[derive(Debug, Default, Clone)]
pub struct TokenStream {
    tokens: Rc<Vec<Token>>,
}

impl TokenStream {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn new_with(tokens: Vec<Token>) -> Self {
        Self {
            tokens: Rc::new(tokens),
        }
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        Rc::make_mut(&mut self.tokens)
    }

    pub fn into_vec(self) -> Vec<Token> {
        match Rc::try_unwrap(self.tokens) {
            Ok(vec) => vec,
            Err(rc) => (*rc).clone(),
        }
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
pub struct TokenStreamReader<'a> {
    span: Span,
    tokens: &'a [Token],
    prev: Option<&'a Token>,
}

impl<'a> TokenStreamReader<'a> {
    pub fn new(span: Span, tokens: &'a [Token]) -> Self {
        Self {
            span,
            tokens,
            prev: None,
        }
    }

    pub fn prev(&self) -> Option<&'a Token> {
        self.prev
    }

    pub fn remaining(&self) -> &'a [Token] {
        self.tokens
    }

    pub fn next_loc(&self) -> FileLoc {
        match self.peek() {
            Some(next) => next.full_span().start(),
            None => self.span.end(),
        }
    }

    pub fn prev_loc(&self) -> FileLoc {
        match self.prev() {
            Some(prev) => prev.full_span().end(),
            None => self.span.start(),
        }
    }
}

impl<'a> StreamReader for TokenStreamReader<'a> {
    type Res = Option<&'a Token>;

    fn consume(&mut self) -> Self::Res {
        if !self.tokens.is_empty() {
            self.prev = Some(&self.tokens[0]);
            self.tokens = &self.tokens[1..];
            self.prev
        } else {
            None
        }
    }
}

impl LookaheadReader for TokenStreamReader<'_> {}

// === IR === //

pub trait AnyToken: Display {
    fn full_span(&self) -> Span;
}

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum Token {
        Group(TokenGroup),
        Ident(TokenIdent),
        Punct(TokenPunct),
        StringLit(TokenStringLit),
        NumberLit(TokenNumberLit),
    }
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

    pub fn full_span(&self) -> Span {
        self.as_any().full_span()
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

    pub fn tokens(&self) -> &[Token] {
        self.stream.tokens()
    }

    pub fn tokens_mut(&mut self) -> &mut Vec<Token> {
        self.stream.tokens_mut()
    }

    pub fn opening_brace(&self) -> FileLoc {
        self.span.start()
    }

    pub fn closing_brace(&self) -> FileLoc {
        self.span.end()
    }

    pub fn reader(&self) -> TokenStreamReader<'_> {
        TokenStreamReader::new(self.span.clone(), self.stream.tokens())
    }
}

impl AnyToken for TokenGroup {
    fn full_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TokenIdent {
    pub span: Span,
    pub text: String,
}

impl TokenIdent {
    pub fn text(&self) -> String {
        self.text.clone()
    }
}

impl AnyToken for TokenIdent {
    fn full_span(&self) -> Span {
        self.span.clone()
    }
}

#[derive(Debug, Clone)]
pub struct TokenPunct {
    pub loc: FileLoc,
    pub is_glued: bool,
    pub char: PunctChar,
}

impl AnyToken for TokenPunct {
    fn full_span(&self) -> Span {
        Span::new(&self.loc, &self.loc)
    }
}

#[derive(Debug, Clone)]
pub struct TokenStringLit {
    pub span: Span,
    pub mode: StringMode,
    pub parts: Vec<StringComponent>,
}

impl AnyToken for TokenStringLit {
    fn full_span(&self) -> Span {
        self.span.clone()
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
    pub exp_part: Option<(bool, String)>,
}

impl AnyToken for TokenNumberLit {
    fn full_span(&self) -> Span {
        self.span.clone()
    }
}

pub const DIGITS_DECIMAL: &'static str = "0123456789";

// Digit char-list is case insensitive.
pub const DIGITS_HEXADECIMAL: &'static str = "0123456789abcdef";

enum_meta! {
    #[derive(Debug)]
    pub enum(GroupDelimiterMeta) GroupDelimiter {
        File = GroupDelimiterMeta {
            name: "file",
            closing_name: "<EOF>",
            encounter_name: "a file delimited group (this should not be possible)",
            left: None,
            right: CharOrEof::Eof,
        },
        Paren = GroupDelimiterMeta {
            name: "parenthesis",
            closing_name: ")",
            encounter_name: "parenthetical group",
            left: Some(CharOrEof::Char('(')),
            right: CharOrEof::Char(')'),
        },
        Brace = GroupDelimiterMeta {
            name: "brace",
            closing_name: "}",
            encounter_name: "braced group",
            left: Some(CharOrEof::Char('{')),
            right: CharOrEof::Char('}'),
        },
        Bracket = GroupDelimiterMeta {
            name: "square bracket",
            encounter_name: "bracketed group",
            closing_name: "]",
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
        Less = '<',
        Greater = '>',
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
    pub name: &'static str,
    pub closing_name: &'static str,
    pub encounter_name: &'static str,
    pub left: Option<CharOrEof>,
    pub right: CharOrEof,
}

#[derive(Debug, Copy, Clone)]
pub struct NumberPrefixMeta {
    pub prefix: Option<char>,
    pub digits: &'static str,
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
        write!(f, "{}", self.char.meta())
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
