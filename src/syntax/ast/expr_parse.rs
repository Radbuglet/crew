use crate::syntax::ast::expr::{AstExpr, AstKeyword};
use crate::syntax::token::{
    GroupDelimiter, PunctChar, Token, TokenGroup, TokenIdent, TokenPunct, TokenStream,
    TokenStreamReader,
};
use crate::util::enum_utils::EnumMeta;
use crate::util::reader::{LookaheadReader, StreamReader};

// === Main parsing logic === //

impl AstExpr {
    pub fn parse(_tokens: &TokenStream) -> Self {
        todo!()
    }
}

// === Atom definitions === //

pub enum StreamAtom {}

#[derive(Debug, Clone)]
pub struct AtomAccessor {
    pub fn_name: TokenIdent,
}

#[derive(Debug, Clone)]
pub struct AtomPath {
    pub is_absolute: bool,
    pub parts: Vec<TokenIdent>,
}

#[derive(Debug, Clone)]
pub struct AtomIf {
    pub primary: AtomIfClause,
    pub secondaries: Vec<AtomIfClause>,
    pub fallback: Option<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AtomIfClause {
    pub cond: AstExpr,
    pub clause: AstExpr,
}

#[derive(Debug, Clone)]
pub struct AtomLoop {
    pub block: AstExpr,
}

// === Atom matching === //

pub fn match_util_punct<'a>(
    reader: &'a mut TokenStreamReader,
    char: PunctChar,
    glued: Option<bool>,
) -> Option<&'a TokenPunct> {
    reader.lookahead(move |reader| match reader.consume() {
        Some(Token::Punct(punct))
            if glued.map_or(false, |val| val == punct.is_glued) && char == punct.char =>
        {
            Some(punct)
        }
        _ => None,
    })
}

pub fn match_util_turbo(reader: &mut TokenStreamReader) -> bool {
    reader.lookahead(|reader| {
        match_util_punct(reader, PunctChar::Colon, None).is_some()
            && match_util_punct(reader, PunctChar::Colon, Some(true)).is_some()
    })
}

#[derive(Debug, Clone)]
pub struct IdentOrKeyword<'a> {
    pub raw: &'a TokenIdent,
    pub keyword: Option<AstKeyword>,
}

pub fn match_util_ident<'a>(reader: &mut TokenStreamReader<'a>) -> Option<IdentOrKeyword<'a>> {
    reader.lookahead(move |reader| {
        let raw = match reader.consume() {
            Some(Token::Ident(ident)) => ident,
            _ => return None,
        };

        let keyword = AstKeyword::values_iter().find_map(|(kw, text)| {
            if raw.text.as_str() == *text {
                Some(kw)
            } else {
                None
            }
        });

        Some(IdentOrKeyword { raw, keyword })
    })
}

pub fn match_util_keyword(reader: &mut TokenStreamReader) -> Option<AstKeyword> {
    match_util_ident(reader).and_then(|id| id.keyword)
}

// TODO: Differentiate between block expressions and parenthesized expressions
pub fn match_simple_expr(
    reader: &mut TokenStreamReader,
    expected_delimiter: GroupDelimiter,
) -> Option<AstExpr> {
    reader.lookahead(|reader| match reader.consume() {
        Some(Token::Group(TokenGroup {
            delimiter, stream, ..
        })) if *delimiter == expected_delimiter => Some(AstExpr::parse(stream)),
        _ => None,
    })
}

pub fn match_accessor(reader: &mut TokenStreamReader) -> Option<AtomAccessor> {
    reader.lookahead(move |reader| {
        // Match period
        if match_util_punct(reader, PunctChar::Period, None).is_none() {
            return None;
        }

        // Match identifier
        let fn_name = match match_util_ident(reader) {
            Some(IdentOrKeyword {
                raw,
                keyword: Option::None,
            }) => raw,
            Some(IdentOrKeyword {
                keyword: Some(_), ..
            }) => panic!("Method name cannot be a reserved keyword!"),
            None => return None,
        };

        Some(AtomAccessor {
            fn_name: fn_name.clone(),
        })
    })
}

pub fn match_path(reader: &mut TokenStreamReader) -> Option<AtomPath> {
    const ERR_INCOMPLETE: &'static str = ":: must be followed by an identifier.";

    fn match_path_part<'a>(reader: &mut TokenStreamReader<'a>) -> Option<&'a TokenIdent> {
        reader.lookahead(|reader| match match_util_ident(reader) {
            Some(IdentOrKeyword {
                raw: first,
                keyword: None,
            }) => Some(first),
            Some(IdentOrKeyword {
                keyword: Some(_), ..
            }) => panic!("Path cannot contain keywords!"),
            _ => return None,
        })
    }

    reader.lookahead(|reader| {
        // Match absolute specifier
        let is_absolute = match_util_turbo(reader);

        let mut parts = Vec::new();

        // Match first component
        match match_path_part(reader) {
            Some(ident) => parts.push(ident.clone()),
            None if is_absolute => panic!("{}", ERR_INCOMPLETE),
            None => return None,
        }

        // Match subsequent components
        reader.consume_while(|reader| {
            // Match separator
            if !match_util_turbo(reader) {
                return false;
            }

            // Match mandatory component
            parts.push(match_path_part(reader).expect(ERR_INCOMPLETE).clone());

            true
        });

        Some(AtomPath { is_absolute, parts })
    })
}

pub fn match_if_expr(reader: &mut TokenStreamReader) -> Option<AtomIf> {
    fn match_if_clause(reader: &mut TokenStreamReader) -> Option<AtomIfClause> {
        reader.lookahead(|reader| {
            // Match if keyword
            if match_util_keyword(reader) != Some(AstKeyword::If) {
                return None;
            }

            // Match condition
            // TODO: Remove need for parens surrounding conditions
            let cond = match_simple_expr(reader, GroupDelimiter::Paren)
                .expect("Expected parenthesized condition after \"if\" keyword.");

            let clause = match_simple_expr(reader, GroupDelimiter::Brace)
                .expect("Expected braced then clause after \"if\" condition.");

            Some(AtomIfClause { cond, clause })
        })
    }

    reader.lookahead(|reader| {
        // Match if clause
        let primary = match match_if_clause(reader) {
            Some(clause) => clause,
            None => return None,
        };

        // Match else-if clauses
        let mut secondaries = Vec::new();
        let trailing_else = loop {
            // Match else
            if match_util_keyword(reader) != Some(AstKeyword::Else) {
                break false;
            }

            // Match if
            if let Some(secondary) = match_if_clause(reader) {
                secondaries.push(secondary);
            } else {
                break true;
            }
        };

        // Match trailing else clause if present
        let fallback = if trailing_else {
            Some(
                match_simple_expr(reader, GroupDelimiter::Brace)
                    .expect("Expected braced then clause after \"else\" condition."),
            )
        } else {
            None
        };

        Some(AtomIf {
            primary,
            secondaries,
            fallback,
        })
    })
}

pub fn match_loop_expr(reader: &mut TokenStreamReader) -> Option<AtomLoop> {
    reader.lookahead(|reader| {
        // Match loop keyword
        if match_util_keyword(reader) != Some(AstKeyword::Loop) {
            return None;
        }

        // Match loop block
        let block = match_simple_expr(reader, GroupDelimiter::Brace)
            .expect("Expected block after \"loop\" keyword.");

        Some(AtomLoop { block })
    })
}

pub fn match_match_expr(_reader: &mut TokenStreamReader) {}

pub fn match_break_expr(_reader: &mut TokenStreamReader) {}

pub fn match_continue_expr(_reader: &mut TokenStreamReader) {}

pub fn match_return_expr(_reader: &mut TokenStreamReader) {}

pub fn match_bin_op_expr(_reader: &mut TokenStreamReader) {}

pub fn match_cast_expr(_reader: &mut TokenStreamReader) {}

pub fn match_paren_expr(_reader: &mut TokenStreamReader) {}

// === Atom folding === //

// TODO
