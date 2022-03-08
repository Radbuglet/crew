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
//! In addition to the benefits of its IR, the tokenizer also provides:
//!
//! - Early interning of identifiers and literals
//! - Unbalanced group recovery
//! - Invalid character recovery
//! - Literal parsing
//! - Doc comment lowering
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

use crate::syntax::diagnostic::Diagnostics;
use crate::syntax::span::{CharOrEof, FileLoc, FileReader, ReadAtom, Span};
use crate::util::backing::Captures;
use crate::util::enum_utils::{enum_categories, enum_meta, EnumMeta, VariantOf};
use crate::util::reader::{match_choice, LookaheadReader, RepFlow, StreamReader};
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

// === Working IR === //

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum DelimiterMode {
    Open,
    Close,
}

enum_categories! {
    #[derive(Clone)]
    enum StackFrame {
        Group(TokenGroup),
        String(TokenStringLit),
        BlockComment,
    }
}

// === Tokenizing === //

pub fn tokenize_file(reader: &mut FileReader, diag: &mut Diagnostics) -> Result<TokenGroup, ()> {
    // Consume any shebang at the beginning of the file.
    match_shebang(reader);

    // Start tokenizing
    let mut stack = vec![StackFrame::Group(TokenGroup::new(
        reader.next_loc().char_span(),
        GroupDelimiter::File,
    ))];

    loop {
        match stack.last_mut().unwrap() {
            // Handle group parsing
            StackFrame::Group(group) => {
                // Match group delimiters
                match group_match_delimiter(reader) {
                    // Group open
                    Some((delimiter, DelimiterMode::Open)) => {
                        stack.push(StackFrame::Group(TokenGroup::new(
                            reader.prev_loc().char_span(),
                            delimiter,
                        )));
                        continue;
                    }
                    // Group close
                    Some((delimiter, DelimiterMode::Close)) => {
                        // Validate delimiter
                        if group.delimiter != delimiter {
                            // TODO: Recover gracefully
                            diag.fatal(
                                Span::new(group.full_span().start(), reader.prev_loc()),
                                format!(
                                    "Unexpected \"{}\". Expected \"{}\"",
                                    delimiter.meta().closing_name,
                                    group.delimiter.meta().closing_name,
                                ),
                            )?;
                        }

                        // Get child (current) group
                        let child = match stack.pop() {
                            Some(StackFrame::Group(group)) => group,
                            _ => unreachable!(),
                        };

                        // Push to parent group
                        match stack.last_mut() {
                            // We're inside another group!
                            Some(StackFrame::Group(parent)) => {
                                parent.tokens_mut().push(child.wrap())
                            }
                            // We're inside a templated string!
                            Some(StackFrame::String(str)) => {
                                str.parts.push(StringComponent::Template(child))
                            }
                            // ...what?
                            Some(StackFrame::BlockComment) => unreachable!(),
                            // We're at the root of the file so we've finished parsing.
                            None => return Ok(child),
                        }

                        continue;
                    }
                    // Nothing, fallthrough.
                    None => {}
                }

                // Sanity check
                debug_assert_ne!(reader.peek(), ReadAtom::Eof);

                // Match string literal start
                if let Some(token) = group_match_string_start(reader) {
                    stack.push(StackFrame::String(token));
                    continue;
                }

                // Match comment start
                // We match comments before punctuation because line comments can be parsed as two
                // '/' puncts back to back and block comments can be parsed as a '/' punct followed
                // by an '*'.
                if group_match_line_comment(reader) {
                    continue;
                }

                if group_match_block_comment_start(reader) {
                    stack.push(StackFrame::BlockComment);
                    continue;
                }

                // Match whitespace
                if group_match_whitespace(reader, false) {
                    continue;
                }

                // Match identifier
                if let Some(ident) = group_match_ident(reader) {
                    group.tokens_mut().push(Token::Ident(ident));
                    continue;
                }

                // Match punctuation
                {
                    let glued_punct = match group.tokens().last() {
                        Some(Token::Punct(_)) => true,
                        _ => false,
                    };

                    if let Some(punct) = group_match_punct(reader, glued_punct) {
                        group.tokens_mut().push(Token::Punct(punct));
                        continue;
                    }
                }

                // Match number literal
                if let Some(num) = group_match_number_lit(reader, diag)? {
                    let num_end = reader.prev_loc();

                    if reader.peek_ahead(group_match_ident).is_some() {
                        diag.display_error(
                            Span::new(num_end, reader.prev_loc()),
                            "Identifier cannot be placed directly after a number literal.",
                        );
                    }

                    group.tokens_mut().push(Token::NumberLit(num));
                    continue;
                }

                diag.display_error(
                    reader.prev_loc().char_span(),
                    "Unexpected character in group.",
                );
            }
            StackFrame::String(string) => {
                let mut text = String::new();
                loop {
                    // Match EOF
                    if reader.peek() == ReadAtom::Eof {
                        diag.fatal(
                            Span::new(string.span.start(), reader.next_loc()),
                            "Unexpected \"<EOF>\" while parsing string literal.",
                        )?;
                    }

                    // Match multiline escape
                    // We match multiline escapes before other character escapes because the latter
                    // does not recognize the newline escape.
                    if string_match_multiline_escape(reader) {
                        continue;
                    }

                    // We match escape sequences before any special delimiter characters because the
                    // escape might be for a delimiter.
                    if let Some(char) = string_match_escape(reader, diag).invert() {
                        if let Ok(char) = char {
                            text.push(char);
                        }
                        continue;
                    }

                    // Match closing quote
                    if string_match_end(reader) {
                        if !text.is_empty() {
                            string.parts.push(StringComponent::Literal(text));
                        }

                        let string = match stack.pop() {
                            Some(StackFrame::String(string)) => string,
                            _ => unreachable!(),
                        };

                        match stack.last_mut() {
                            Some(StackFrame::Group(group)) => {
                                group.tokens_mut().push(string.wrap())
                            }
                            Some(StackFrame::String(_)) => unreachable!(),
                            Some(StackFrame::BlockComment) => unreachable!(),
                            None => unreachable!(),
                        }

                        break;
                    }

                    // Match placeholder
                    if string.mode == StringMode::Templated
                        && string_match_placeholder_start(reader)
                    {
                        if !text.is_empty() {
                            string.parts.push(StringComponent::Literal(text));
                        }

                        stack.push(
                            TokenGroup::new(
                                Span::new(&reader.prev_loc(), &reader.prev_loc()),
                                GroupDelimiter::Brace,
                            )
                            .wrap(),
                        );

                        break;
                    }

                    // Match character
                    if let Some(char) = string_match_char(reader) {
                        text.push(char);
                        continue;
                    }

                    diag.display_error(
                        reader.prev_loc().char_span(),
                        "Unexpected character in string.",
                    );
                }
            }
            StackFrame::BlockComment => {
                if group_match_block_comment_start(reader) {
                    stack.push(StackFrame::BlockComment);
                    continue;
                }

                if comment_match_block_end(reader) {
                    stack.pop();
                    continue;
                }

                if reader.peek() == ReadAtom::Eof {
                    diag.fatal(
                        reader.next_loc().char_span(),
                        "Unexpected \"<EOF>\" while parsing block quote.",
                    )?;
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

        let char = PunctChar::find_where(move |_, codepoint| read == *codepoint)?;

        Some(TokenPunct {
            loc: start,
            is_glued,
            char,
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

pub fn tokenize_read_digits<'a, 'b>(
    reader: &'a mut FileReader<'b>,
    digits: &'b str,
) -> impl Iterator<Item = char> + Captures<'b> + 'a {
    fn alphabet_contains(alphabet: &str, char: char) -> bool {
        let char = char.to_ascii_lowercase();
        alphabet.chars().any(|allowed| allowed == char)
    }

    std::iter::from_fn(move || {
        reader
            .consume_while(|reader| match reader.consume() {
                ReadAtom::Codepoint(char) if alphabet_contains(digits, char) => {
                    RepFlow::Finish(char.to_ascii_lowercase())
                }
                ReadAtom::Codepoint('_') => RepFlow::Continue('_'),
                _ => RepFlow::Reject,
            })
            .last()
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
///     ("." + DEC_DIGIT*)?
///     [exponent?: "E" + (+|-) + DEC_DIGIT*]
/// ]
/// ```
///
/// **Note:** we explicitly do not parse the leading sign because it will be parsed as a unary
/// operator on the numeric literal.
///
/// See [NumberPrefix] for more information about prefixes.
///
pub fn group_match_number_lit(
    reader: &mut FileReader,
    diag: &mut Diagnostics,
) -> OldBarePResult<TokenNumberLit> {
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
                NumberPrefix::find_where(move |_, meta| Some(read) == meta.prefix)
            })
            .unwrap_or(NumberPrefix::Decimal);

        // Read integer-part digits
        let int_part = {
            let text = tokenize_read_digits(&mut *reader, prefix.meta().digits).collect::<String>();

            if text.is_empty() {
                return if prefix == NumberPrefix::Decimal {
                    Ok(None)
                } else {
                    diag.fatal(
                        reader.prev_loc().char_span(),
                        format!(
                            "Expected digits after explicit prefix (one of \"{}\")!",
                            prefix.meta().digits
                        ),
                    )?;
                };
            }
            text
        };

        // Read floating-part digits
        let float_part = {
            // Detect decimal point but ignore adjoined range expressions ("..")
            let has_fp = {
                // Save reader state before matching periods.
                let reader_no_fp = reader.clone();

                // Match decimal point.
                let has_fp = match_str(reader, ".");

                // Match second period to disambiguate between trailing floating-points and range
                // expressions.
                if match_str(reader, ".") {
                    *reader = reader_no_fp;
                    return Ok(Some(TokenNumberLit {
                        prefix,
                        span: Span::new(&start, &reader.prev_loc()),
                        int_part,
                        float_part: None,
                        exp_part: None,
                    }));
                }

                has_fp
            };

            if has_fp {
                if prefix != NumberPrefix::Decimal {
                    diag.fatal(
                        reader.prev_loc().char_span(),
                        "Cannot specify a floating part for non-decimal numbers.",
                    )?;
                }

                Some(tokenize_read_digits(reader, &DIGITS_DECIMAL).collect())
            } else {
                None
            }
        };

        // Read exponents
        let exp_part = if match_str(reader, "E") {
            // Match sign (optional)
            let is_positive = match_choice!(
                &mut *reader,
                |reader| match_str(reader, "+").then_some(true),
                |reader| match_str(reader, "-").then_some(false),
            )
            .unwrap_or(true);

            // Match digits
            let digits = tokenize_read_digits(reader, &DIGITS_DECIMAL).collect::<String>();

            if digits.is_empty() {
                diag.fatal(
                    reader.prev_loc().char_span(),
                    "Explicit exponent must be followed by one or more decimal digits.",
                )?;
            }

            Some((is_positive, digits))
        } else {
            None
        };

        Ok(Some(TokenNumberLit {
            prefix,
            span: Span::new(&start, &reader.prev_loc()),
            int_part,
            float_part,
            exp_part,
        }))
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

pub fn string_match_escape(
    reader: &mut FileReader,
    diag: &mut Diagnostics,
) -> OldBarePResult<char> {
    reader.lookahead(|reader| {
        let escape_start = reader.next_loc();

        if match_str(reader, "\\") {
            let escape = match reader.consume() {
                // ASCII escapes
                ReadAtom::Codepoint('r') => '\r',
                ReadAtom::Codepoint('n') => '\n',
                ReadAtom::Codepoint('t') => '\t',
                ReadAtom::Codepoint('0') => '\0',

                // Delimiter escapes
                ReadAtom::Codepoint('\\') => '\\',
                ReadAtom::Codepoint('"') => '"',
                ReadAtom::Codepoint('\'') => '\'',
                ReadAtom::Codepoint('{') => '{',

                // Codepoints
                ReadAtom::Codepoint('x') => {
                    let hex = tokenize_read_digits(reader, DIGITS_HEXADECIMAL)
                        .take(2)
                        .collect::<String>();

                    if hex.len() != 2 {
                        diag.fatal(
                            Span::new(escape_start, reader.prev_loc()),
                            "ASCII character escapes expect exactly two hexadecimal digits."
                        )?;
                    }

                    let char = char::from_u32(u8::from_str_radix(&hex, 16).unwrap() as u32);

                    match char {
                        Some(char) if char.is_ascii() => char,
                        _ => diag.fatal(
                            Span::new(escape_start, reader.prev_loc()),
                            format!("ASCII character escape 0x{} must be less than 0x7F (the highest ASCII character)", hex)
                        )?,
                    }
                }
                ReadAtom::Codepoint('u') => {
                    if !match_str(reader, "{") {
                        diag.fatal(
                            Span::new(escape_start, reader.prev_loc()),
                            "Expected `{` after start of Unicode escape (`\\u`)",
                        )?;
                    }

                    let hex = tokenize_read_digits(reader, DIGITS_HEXADECIMAL).take(6).collect::<String>();

                    if !match_str(reader, "}") {
                        diag.fatal(
                            Span::new(escape_start, reader.prev_loc()),
                            "Expected `}` at the end of Unicode escape.",
                        )?;
                    }

                    let char = char::from_u32(u32::from_str_radix(&hex, 16).unwrap());

                    match char {
                        Some(char) => char,
                        _ => diag.fatal(
                            Span::new(escape_start, reader.prev_loc()),
                            format!("Unicode character escape \\u{{{}}} forms an invalid codepoint.", hex),
                        )?,
                    }
                },
                _ => diag.fatal(
                    Span::new(escape_start, reader.prev_loc()),
                    "Invalid escape code.",
                )?,
            };
            Ok(Some(escape))
        } else {
            Ok(None)
        }
    })
}

pub fn string_match_char(reader: &mut FileReader) -> Option<char> {
    reader.lookahead(|reader| match reader.consume() {
        ReadAtom::Codepoint(char) => Some(char),
        ReadAtom::Newline(_) => Some('\n'),
        _ => None,
    })
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

// === Legacy error handling === //

// TODO: Use new diagnostic system.

type OldPResult<T, E> = Result<Option<T>, E>;
type OldBarePResult<T> = OldPResult<T, ()>;

trait PResultOptionExt {
    type Value;

    fn ok_or_unmatched<E>(self) -> OldPResult<Self::Value, E>;

    fn ok_or_with_error<F, E>(self, on_missing: F) -> OldPResult<Self::Value, E>
    where
        F: FnMut() -> E;

    fn ok_or_error<E>(self, err: E) -> OldPResult<Self::Value, E>;

    fn ok_or_default_error<E: Default>(self) -> OldPResult<Self::Value, E>;
}

impl<T> PResultOptionExt for Option<T> {
    type Value = T;

    fn ok_or_unmatched<E>(self) -> OldPResult<Self::Value, E> {
        match self {
            Some(value) => Ok(Some(value)),
            None => Ok(None),
        }
    }

    fn ok_or_with_error<F, E>(self, mut on_missing: F) -> OldPResult<Self::Value, E>
    where
        F: FnMut() -> E,
    {
        match self {
            Some(value) => Ok(Some(value)),
            None => Err(on_missing()),
        }
    }

    fn ok_or_error<E>(self, err: E) -> OldPResult<Self::Value, E> {
        match self {
            Some(value) => Ok(Some(value)),
            None => Err(err),
        }
    }

    fn ok_or_default_error<E: Default>(self) -> OldPResult<Self::Value, E> {
        self.ok_or_with_error(Default::default)
    }
}

pub trait PResultExt {
    type Success;
    type Error;

    fn invert(self) -> Option<Result<Self::Success, Self::Error>>;

    fn expect_with<F>(self, on_missing: F) -> Result<Self::Success, Self::Error>
    where
        F: FnMut() -> Self::Error;

    fn expect(self, on_missing: Self::Error) -> Result<Self::Success, Self::Error>;

    fn expect_default(self) -> Result<Self::Success, Self::Error>
    where
        Self::Error: Default;
}

impl<T, E> PResultExt for OldPResult<T, E> {
    type Success = T;
    type Error = E;

    fn invert(self) -> Option<Result<Self::Success, Self::Error>> {
        match self {
            Ok(Some(present)) => Some(Ok(present)),
            Ok(None) => None,
            Err(err) => Some(Err(err)),
        }
    }

    fn expect_with<F>(self, mut on_missing: F) -> Result<Self::Success, Self::Error>
    where
        F: FnMut() -> Self::Error,
    {
        self.invert().unwrap_or_else(|| Err(on_missing()))
    }

    fn expect(self, on_missing: Self::Error) -> Result<Self::Success, Self::Error> {
        self.invert().unwrap_or(Err(on_missing))
    }

    fn expect_default(self) -> Result<Self::Success, Self::Error>
    where
        Self::Error: Default,
    {
        self.expect_with(Default::default)
    }
}

pub trait DiagnosticsFatalExt {
    fn fatal<F: Display>(&mut self, span: Span, text: F) -> Result<!, ()>;
}

impl DiagnosticsFatalExt for Diagnostics {
    fn fatal<F: Display>(&mut self, span: Span, text: F) -> Result<!, ()> {
        self.display_error(span, text);
        Err(())
    }
}
