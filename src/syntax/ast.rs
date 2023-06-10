use bumpalo::Bump;

use crate::util::parser::{Cursor, StreamCursor};

use super::token::{
    GroupDelimiter, TokenCursor, TokenGroup, TokenHinter, TokenIdent, TokenParser, TokenResult,
    TokenUnexpectedFmt,
};

// === IR === //

#[derive(Debug, Clone)]
pub struct AstModule<'a> {
    pub items: &'a [AstModuleItem<'a>],
}

#[derive(Debug, Clone)]
pub enum AstModuleItem<'a> {
    Struct(&'a AstStruct<'a>),
    Func(&'a AstFunc<'a>),
}

#[derive(Debug, Clone)]
pub struct AstStruct<'a> {
    pub name: &'a str,
    pub fields: &'a [&'a AstFieldLike<'a>],
}

#[derive(Debug, Clone)]
pub struct AstFieldLike<'a> {
    pub name: &'a str,
    pub ty: &'a AstType<'a>,
}

#[derive(Debug, Clone)]
pub struct AstType<'a> {
    pub name: &'a str,
}

#[derive(Debug, Clone)]
pub struct AstFunc<'a> {
    pub name: &'a str,
    pub params: &'a [&'a AstFieldLike<'a>],
    pub ret: Option<&'a AstType<'a>>,
    pub body: (),
}

#[derive(Debug, Clone)]
pub struct AstFuncBlock<'a> {
    pub stmts: &'a [AstStmt<'a>],
}

#[derive(Debug, Clone)]
pub enum AstStmt<'a> {
    DeclVar(&'a AstStmtDeclVar<'a>),
    SetVar(&'a AstStmtSetVar<'a>),
    Expr(AstExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct AstStmtDeclVar<'a> {
    pub name: &'a str,
    pub ty: &'a AstType<'a>,
    pub init: AstExpr<'a>,
}

#[derive(Debug, Clone)]
pub struct AstStmtSetVar<'a> {
    pub name: &'a str,
    pub value: AstExpr<'a>,
}

#[derive(Debug, Clone)]
pub enum AstExpr<'a> {
    Foo(&'a str),
}

// === Parser === //

fn is_reserved(id: &str) -> bool {
    matches!(id, "struct" | "fn")
}

fn match_keyword(kw: &'static str) -> impl Fn(&mut TokenCursor<'_>) -> bool {
    assert!(is_reserved(kw));

    move |c| {
        c.lookahead(|c| {
            c.consume()
                .and_then(|t| t.as_ident())
                .filter(|t| t.name == kw)
                .is_some()
        })
    }
}

fn match_ident<'a>(
    c: &mut TokenCursor<'a>,
    h: &mut TokenHinter<'_, 'a>,
) -> Option<&'a TokenIdent<'a>> {
    c.lookahead(|c| {
        let start = c.clone();
        let ident = c.consume()?.as_ident()?;
        if is_reserved(ident.name) {
            h.hint_spanned(
                start.span_one(),
                format!("{:?} is a reserved keyword.", ident.name),
            );
            return None;
        }

        Some(ident)
    })
}

fn match_punct(char: char) -> impl Fn(&mut TokenCursor<'_>) -> bool {
    move |c| {
        c.lookahead(|c| {
            c.consume()
                .and_then(|t| t.as_punct())
                .filter(|t| t.char == char)
                .is_some()
        })
    }
}

fn match_punct_seq(char: &'static str) -> impl Fn(&mut TokenCursor<'_>) -> bool {
    move |c| c.lookahead(|c| char.chars().all(|char| match_punct(char)(c)))
}

fn match_group(
    delimiter: GroupDelimiter,
) -> impl for<'a> Fn(&mut TokenCursor<'a>) -> Option<&'a TokenGroup<'a>> {
    move |c| {
        c.lookahead(|c| {
            c.consume()
                .and_then(|t| t.as_group())
                .filter(|g| g.delimiter == delimiter)
        })
    }
}

pub fn parse_module<'a>(
    bump: &'a Bump,
    p: &mut TokenParser<'a>,
) -> TokenResult<'a, &'a AstModule<'a>> {
    let mut items = Vec::new();

    loop {
        if p.expecting("EOF").try_match(|c| c.consume().is_none()) {
            break;
        }

        if p.expecting("struct").try_match(match_keyword("struct")) {
            let Some(name) = p.expecting("struct name").try_match_hinted(match_ident) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            let Some(fields_tt) = p.expecting("{").try_match(match_group(GroupDelimiter::Brace)) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            let fields = parse_field_like_list(bump, &mut fields_tt.parser(), "}")?;

            items.push(AstModuleItem::Struct(bump.alloc(AstStruct {
                name: name.name,
                fields,
            })));

            continue;
        }

        if p.expecting("fn").try_match(match_keyword("fn")) {
            let Some(name) = p.expecting("function name").try_match_hinted(match_ident) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            let Some(args_tt) = p.expecting("(").try_match(match_group(GroupDelimiter::Parenthesis)) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            let params = parse_field_like_list(bump, &mut args_tt.parser(), ")")?;

            let ret = if p.expecting("->").try_match(match_punct_seq("->")) {
                Some(parse_type(bump, p)?)
            } else {
                None
            };

            let Some(body) = p.expecting("{").try_match(match_group(GroupDelimiter::Brace)) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            items.push(AstModuleItem::Func(bump.alloc(AstFunc {
                name: name.name,
                params,
                ret,
                body: (),
            })));

            continue;
        }

        return Err(p.unexpected(TokenUnexpectedFmt));
    }

    Ok(bump.alloc(AstModule {
        items: bump.alloc_slice_clone(&items),
    }))
}

pub fn parse_field_like_list<'a>(
    bump: &'a Bump,
    p: &mut TokenParser<'a>,
    closer: &'a str,
) -> TokenResult<'a, &'a [&'a AstFieldLike<'a>]> {
    let mut fields = Vec::new();
    let mut had_comma = true;

    loop {
        if p.expecting(closer).try_match(|c| c.consume().is_none()) {
            break;
        }

        if !had_comma {
            p.hint("Fields must be delimited by a comma.");
            return Err(p.unexpected(TokenUnexpectedFmt));
        }

        let Some(name) = p.expecting("field name").try_match_hinted(match_ident) else {
			return Err(p.unexpected(TokenUnexpectedFmt));
		};

        if !p.expecting(":").try_match(match_punct(':')) {
            return Err(p.unexpected(TokenUnexpectedFmt));
        }

        let ty = parse_type(bump, p)?;

        fields.push(&*bump.alloc(AstFieldLike {
            name: name.name,
            ty,
        }));

        had_comma = p.expecting(",").try_match(match_punct(','));
    }

    Ok(bump.alloc_slice_copy(&fields))
}

pub fn parse_type<'a>(bump: &'a Bump, p: &mut TokenParser<'a>) -> TokenResult<'a, &'a AstType<'a>> {
    let Some(ident) = p.expecting("type name").try_match_hinted(match_ident) else {
		return Err(p.unexpected(TokenUnexpectedFmt));
	};

    Ok(bump.alloc(AstType { name: ident.name }))
}
