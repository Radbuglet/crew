use bumpalo::Bump;

use crate::util::parser::{Cursor, ParseError, StreamCursor};

use super::token::{
    GroupDelimiter, TokenCursor, TokenGroup, TokenHinter, TokenIdent, TokenParser, TokenResult,
    TokenUnexpectedFmt,
};

// === IR === //

// FIXME: Ensure that bump allocated values are actually dropped if they need it.

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
    pub body: &'a AstBlock<'a>,
}

#[derive(Debug, Clone)]
pub struct AstBlock<'a> {
    pub stmts: &'a [AstStmt<'a>],
}

#[derive(Debug, Clone)]
pub enum AstStmt<'a> {
    DeclVar(&'a AstStmtDeclVar<'a>),
    OpVar(&'a AstStmtOpVar<'a>),
    Expr(AstExpr<'a>),
}

#[derive(Debug, Clone)]
pub struct AstStmtDeclVar<'a> {
    pub name: &'a str,
    pub is_mut: bool,
    pub ty: Option<&'a AstType<'a>>,
    pub init: Option<AstExpr<'a>>,
}

#[derive(Debug, Clone)]
pub struct AstStmtOpVar<'a> {
    pub name: &'a str,
    pub op: AstStmtVarOp,
    pub value: AstExpr<'a>,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AstStmtVarOp {
    Add,
    Sub,
    Mul,
    Div,
    And,
    Xor,
    Or,
}

#[derive(Debug, Clone)]
pub enum AstExpr<'a> {
    BinOp(AstExprBinOp<'a>),
    UnOp(AstExprUnOp<'a>),
    Var(&'a TokenIdent<'a>),
    Block(&'a AstBlock<'a>),
    Paren(&'a AstExpr<'a>),
    Loop(&'a AstBlock<'a>),
}

#[derive(Debug, Clone)]
pub struct AstExprBinOp<'a> {
    pub left: &'a AstExpr<'a>,
    pub right: &'a AstExpr<'a>,
    pub operator: AstExprBinOperator,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AstExprBinOperator {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Xor,
    Or,
    And,
}

#[derive(Debug, Clone)]
pub struct AstExprUnOp<'a> {
    pub target: &'a AstExpr<'a>,
    pub operator: AstExprUnOperator,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum AstExprUnOperator {
    Neg,
}

// === Parser Primitives === //

fn is_reserved(id: &str) -> bool {
    matches!(
        id,
        "struct" | "fn" | "let" | "loop" | "if" | "break" | "return" | "mut" | "while"
    )
}

fn match_keyword(kw: &'static str) -> impl Fn(&mut TokenCursor<'_>) -> bool {
    assert!(is_reserved(kw), "{kw} is not a reserved keyword!");

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

// === Parser elements === //

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

            let body = parse_block(bump, &mut body.parser())?;

            items.push(AstModuleItem::Func(bump.alloc(AstFunc {
                name: name.name,
                params,
                ret,
                body,
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

pub fn parse_block<'a>(
    bump: &'a Bump,
    p: &mut TokenParser<'a>,
) -> TokenResult<'a, &'a AstBlock<'a>> {
    let mut stmts = Vec::new();
    'block_parse: loop {
        if p.expecting("}").try_match(|c| c.consume().is_none()) {
            break;
        }

        if p.expecting("let").try_match(match_keyword("let")) {
            let is_mut = p.expecting("mut").try_match(match_keyword("mut"));

            let Some(name) = p.expecting("variable name").try_match_hinted(match_ident) else {
				return Err(p.unexpected(TokenUnexpectedFmt));
			};

            let ty = if p.expecting(":").try_match(match_punct(':')) {
                Some(parse_type(bump, p)?)
            } else {
                None
            };

            let init = if p.expecting("=").try_match(match_punct('=')) {
                Some(parse_expr(bump, p)?)
            } else {
                None
            };

            if !p.expecting(";").try_match(match_punct(';')) {
                return Err(p.unexpected(TokenUnexpectedFmt));
            }

            stmts.push(AstStmt::DeclVar(bump.alloc(AstStmtDeclVar {
                name: name.name,
                is_mut,
                ty,
                init,
            })));

            continue;
        }

        if let Some(ident) = p.expecting("identifier").try_match_hinted(match_ident) {
            // Try to match an operator
            for (seq, op) in [
                ("+=", AstStmtVarOp::Add),
                ("-=", AstStmtVarOp::Sub),
                ("*=", AstStmtVarOp::Mul),
                ("/=", AstStmtVarOp::Div),
                ("&=", AstStmtVarOp::And),
                ("|=", AstStmtVarOp::Or),
                ("^=", AstStmtVarOp::Xor),
            ] {
                if p.expecting(seq).try_match(match_punct_seq(seq)) {
                    let value = parse_expr(bump, p)?;
                    if !p.expecting(";").try_match(match_punct(';')) {
                        return Err(p.unexpected(TokenUnexpectedFmt));
                    }

                    stmts.push(AstStmt::OpVar(bump.alloc(AstStmtOpVar {
                        name: ident.name,
                        op,
                        value,
                    })));
                    continue 'block_parse;
                }
            }

            // Try to match an expression
            let expr = parse_expr_after_identifier(bump, p, ident)?;
            if !p.expecting(";").try_match(match_punct(';')) {
                return Err(p.unexpected(TokenUnexpectedFmt));
            }
            stmts.push(AstStmt::Expr(expr));
            continue;
        }

        // Try to match an expression
        let expr = parse_expr_not_identifier(bump, p)?;
        if !p.expecting(";").try_match(match_punct(';')) {
            return Err(p.unexpected(TokenUnexpectedFmt));
        }
        stmts.push(AstStmt::Expr(expr));
    }

    Ok(bump.alloc(AstBlock {
        stmts: bump.alloc_slice_clone(&stmts),
    }))
}

pub fn parse_expr<'a>(bump: &'a Bump, p: &mut TokenParser<'a>) -> TokenResult<'a, AstExpr<'a>> {
    if let Some(ident) = p.expecting("identifier").try_match_hinted(match_ident) {
        return parse_expr_after_identifier(bump, p, ident);
    }

    parse_expr_not_identifier(bump, p)
}

pub fn parse_expr_not_identifier<'a>(
    bump: &'a Bump,
    p: &mut TokenParser<'a>,
) -> TokenResult<'a, AstExpr<'a>> {
    if let Some(group) = p
        .expecting("{")
        .try_match(match_group(GroupDelimiter::Brace))
    {
        return Ok(AstExpr::Block(parse_block(bump, &mut group.parser())?));
    }

    if p.expecting("if").try_match(match_keyword("if")) {
        let pred = parse_expr(bump, p)?;
        let Some(truthy_tt) = p.expecting("{").try_match(match_group(GroupDelimiter::Brace)) else {
			return Err(p.unexpected(TokenUnexpectedFmt));
		};
        let truthy_block = parse_block(bump, &mut truthy_tt.parser())?;

        return Err(ParseError::new_invalid(
            p.cursor().span_one(),
            "this is not implemented",
        ));
    }

    if p.expecting("loop").try_match(match_keyword("loop")) {
        let Some(loop_tt) = p
            .expecting("{")
            .try_match(match_group(GroupDelimiter::Brace))
		else {
			return Err(p.unexpected(TokenUnexpectedFmt));
		};

        let block = parse_block(bump, &mut loop_tt.parser())?;

        return Ok(AstExpr::Loop(bump.alloc(block)));
    }

    Err(p.unexpected(TokenUnexpectedFmt))
}

pub fn parse_expr_after_identifier<'a>(
    bump: &'a Bump,
    p: &mut TokenParser<'a>,
    ident: &'a TokenIdent,
) -> TokenResult<'a, AstExpr<'a>> {
    Ok(AstExpr::Var(ident))
}
