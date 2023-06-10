use bumpalo::Bump;
use unicode_xid::UnicodeXID;

use crate::util::parser::{
    ParserHinter, SliceCursor, SliceParseError, SliceParser, SliceResult, SpanCursor, StrParser,
    StrResult, StrUnexpectedFromSpan, StreamCursor, UnexpectedFormatter,
};

// === IR === //

#[derive(Debug, Clone)]
pub enum Token<'a> {
    Group(&'a TokenGroup<'a>),
    Ident(&'a TokenIdent<'a>),
    Punct(&'a TokenPunct),
}

impl<'a> Token<'a> {
    pub fn as_group(&self) -> Option<&'a TokenGroup<'a>> {
        match self {
            Self::Group(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_ident(&self) -> Option<&'a TokenIdent<'a>> {
        match self {
            Self::Ident(v) => Some(v),
            _ => None,
        }
    }

    pub fn as_punct(&self) -> Option<&'a TokenPunct> {
        match self {
            Self::Punct(v) => Some(v),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokenGroup<'a> {
    pub delimiter: GroupDelimiter,
    pub tokens: &'a [Token<'a>],
}

impl<'a> TokenGroup<'a> {
    pub fn parser(&self) -> TokenParser<'a> {
        TokenParser::new(SliceCursor::new(self.tokens))
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum GroupDelimiter {
    Virtual,
    Bracket,
    Brace,
    Parenthesis,
}

#[derive(Debug, Clone)]
pub struct TokenIdent<'a> {
    pub name: &'a str,
}

#[derive(Debug, Clone)]
pub struct TokenPunct {
    pub char: char,
    pub glued: bool,
}

impl TokenPunct {
    pub fn is_valid_punct_char(char: char) -> bool {
        const LEGAL_CHARS: &[char] = &[
            '=', '<', '>', '!', '~', '+', '-', '*', '/', '%', '^', '&', '|', '@', '.', ',', ';',
            ':', '#', '$', '?', '\'',
        ];

        LEGAL_CHARS.contains(&char)
    }
}

// === Parser === //

pub type TokenParser<'a> = SliceParser<'a, Token<'a>>;

pub type TokenParseError<'a> = SliceParseError<'a, Token<'a>>;

pub type TokenResult<'a, T> = SliceResult<'a, T, Token<'a>>;

pub type TokenCursor<'a> = SliceCursor<'a, Token<'a>>;

pub type TokenHinter<'a, 'm> = ParserHinter<'a, 'm, TokenCursor<'m>>;

#[derive(Debug, Copy, Clone, Default)]
pub struct TokenUnexpectedFmt;

impl<'a> UnexpectedFormatter<TokenCursor<'a>> for TokenUnexpectedFmt {
    fn format(self, span: &SpanCursor<TokenCursor<'a>>) -> String {
        match span.peek() {
            Some(Token::Group(group)) => match group.delimiter {
                GroupDelimiter::Virtual => "virtual group",
                GroupDelimiter::Bracket => "[",
                GroupDelimiter::Brace => "{",
                GroupDelimiter::Parenthesis => "(",
            }
            .to_string(),
            Some(Token::Ident(ident)) => ident.name.to_string(),
            Some(Token::Punct(punct)) => punct.char.to_string(),
            None => "?".to_string(),
        }
    }
}

// === Tokenizer === //

pub fn tokenize<'a>(bump: &'a Bump, p: &mut StrParser<'a>) -> StrResult<'a, TokenGroup<'a>> {
    tokenize_group(bump, p, GroupDelimiter::Virtual)
}

fn tokenize_group<'a>(
    bump: &'a Bump,
    p: &mut StrParser<'a>,
    delimiter: GroupDelimiter,
) -> StrResult<'a, TokenGroup<'a>> {
    let mut tokens = Vec::new();
    let mut just_matched_punct = false;

    let (del_char, del_name) = match delimiter {
        GroupDelimiter::Virtual => (None, "EOF"),
        GroupDelimiter::Bracket => (Some(']'), "closing bracket"),
        GroupDelimiter::Brace => (Some('}'), "closing curly brace"),
        GroupDelimiter::Parenthesis => (Some(')'), "closing parenthesis"),
    };

    loop {
        // Try to match the closing delimiter
        if p.expecting(del_name).try_match(|c| c.consume() == del_char) {
            break;
        }

        // Try to match a comment
        if p.expecting("//")
            .try_match(|c| c.consume() == Some('/') && c.consume() == Some('/'))
        {
            while let Some(_) = p
                .expecting("comment character")
                .try_match(|c| c.consume().filter(|c| *c != '\n'))
            {}

            // Consume the newline to treat it as if it were part of the comment. This just ensures
            // that "comment character" shows up in the suggestion list.
            p.cursor_mut().consume();

            continue;
        }

        // Try to match an identifier
        if let Some(first) = p
            .expecting("identifier")
            .try_match(|c| c.consume().filter(|c| c.is_xid_start()))
        {
            just_matched_punct = false;

            let mut ident = String::new();
            ident.push(first);

            while let Some(next) = p
                .expecting("identifier character")
                .try_match(|c| c.consume().filter(|c| c.is_xid_continue()))
            {
                ident.push(next);
            }

            tokens.push(Token::Ident(bump.alloc(TokenIdent {
                name: bump.alloc_str(&ident),
            })));

            continue;
        }

        // Try to match a punctuation
        if let Some(char) = p
            .expecting("punctuation")
            .try_match(|c| c.consume().filter(|c| TokenPunct::is_valid_punct_char(*c)))
        {
            tokens.push(Token::Punct(bump.alloc(TokenPunct {
                char,
                glued: just_matched_punct,
            })));
            just_matched_punct = true;

            continue;
        }

        // Try to match a group
        if let Some(delimiter) = p
            .expecting("group delimiter")
            .try_match(|c| match c.consume() {
                Some('[') => Some(GroupDelimiter::Bracket),
                Some('{') => Some(GroupDelimiter::Brace),
                Some('(') => Some(GroupDelimiter::Parenthesis),
                _ => None,
            })
        {
            just_matched_punct = false;
            tokens.push(Token::Group(
                bump.alloc(tokenize_group(bump, p, delimiter)?),
            ));

            continue;
        }

        // Try to match a whitespace
        if p.expecting("whitespace")
            .try_match(|c| c.consume().filter(|c| c.is_whitespace()).is_some())
        {
            just_matched_punct = false;
            continue;
        }

        // Otherwise, raise an error
        return Err(p.unexpected(StrUnexpectedFromSpan));
    }

    let tokens = bump.alloc_slice_clone(tokens.as_slice());
    Ok(TokenGroup { delimiter, tokens })
}
