#![allow(unused_variables)]

use crate::syntax::parse::class_expr::{AstFieldType, AstFuncDef};
use crate::syntax::parse::module::AstModItem;
use crate::syntax::parse::path::AstPathDirect;
use crate::syntax::parse::ty::AstType;
use crate::syntax::parse::util::{
    util_match_ellipsis, util_match_eof, util_match_group_delimited, util_match_ident,
    util_match_num_lit, util_match_punct, util_match_specific_kw, util_match_str_lit,
    util_punct_matcher, AstKeyword, TokenLitSeq,
};
use crate::syntax::token::{
    GroupDelimiter, PunctChar, StringComponent, TokenNumberLit, TokenStreamReader,
};
use crate::util::enum_utils::{enum_categories, enum_meta, EnumMeta, VariantOf};
use crate::util::iter_ext::TryCollectExt;
use crate::util::reader::{match_choice, DelimiterMatcher, LookaheadReader};
use std::rc::Rc;

// === Scopes === //

#[derive(Debug, Clone)]
pub struct AstScope {
    pub items: Vec<AstScopeItem>,
    pub terminator: Option<Rc<AstExpr>>,
}

impl AstScope {
    pub fn parse_braced(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let group = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            Self::parse_inner(&mut group.reader())
        })
    }

    pub fn parse_inner(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let items = reader.consume_while(AstScopeItem::parse).collect();
            let terminator = AstExpr::parse_maybe(reader)?.map(Rc::new);
            util_match_eof(reader)?;

            Some(Self { items, terminator })
        })
    }
}

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstScopeItem {
        // TODO: Assignment operators, optional semicolons for certain expressions.
        Mod(AstModItem),
        Var(AstScopeVariable),
        Stmt(AstScopeStmt),
    }
}

impl AstScopeItem {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        match_choice!(
            reader,
            // Mod
            |reader| Some(AstModItem::parse(reader)?.wrap()),
            // Var
            |reader| Some(AstScopeVariable::parse(reader)?.wrap()),
            // Stmt
            |reader| Some(AstScopeStmt::parse(reader)?.wrap()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct AstScopeVariable {
    pub prefix: AstFieldType,
    pub name: String,
    pub ty: Option<AstType>,
    pub initializer: Option<AstExpr>,
}

impl AstScopeVariable {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let prefix = AstFieldType::parse(reader)?;
            let name = util_match_ident(reader)?;
            let ty = if util_match_punct(reader, PunctChar::Colon, None).is_some() {
                Some(AstType::parse(reader)?)
            } else {
                None
            };

            let initializer = if util_match_punct(reader, PunctChar::Equals, None).is_some() {
                let expr = AstExpr::parse(reader)?;
                util_match_punct(reader, PunctChar::Semicolon, None)?;
                Some(expr)
            } else {
                None
            };

            Some(Self {
                prefix,
                name: name.text(),
                ty,
                initializer,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstScopeStmt {
    pub expr: AstExpr,
}

impl AstScopeStmt {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let expr = AstExpr::parse(reader)?;
            util_match_punct(reader, PunctChar::Semicolon, None)?;
            Some(Self { expr })
        })
    }
}

// === Expressions === //

enum_categories! {
    /// Expression parsing is done in two passes: atomization and folding. Atomization (partially)
    /// consumes a token stream and splits it up into unlinked expressions. These unlinked expressions
    /// are then folded into a single expression tree during the folding parsing phase. Atomization
    /// is highly contextual and many atoms are disambiguated depending on whether they are preceded
    /// by a `cont` i.e. a freestanding expression (e.g. a method/field chain is a `cont`, a binary
    /// operator is not).
    #[derive(Debug, Clone)]
    pub enum AstExpr {
        /// Fetches an object from a path.
        /// Flags: `cont`.
        Fetch(AstExprFetch),

        /// Indexes an object. There is no list initializer syntax for which this can be confused with
        /// (this language prefers macro-based initializers instead).
        /// Flags: `cont`, `requires_cont`
        Index(AstExprIndex),

        /// Accesses an object's field.
        /// Flags: `cont`, `requires_cont`
        Field(AstExprField),

        /// Accesses an object's field. This is potentially confusable with a parenthetical expression
        /// and must be distinguished based off `cont` context.
        /// Flags: `cont`, `requires_cont`
        Call(AstExprCall),

        /// To-Do ellipsis syntax. Panics on reached.
        /// Flags: none
        Todo(AstExprTodo),

        /// A unary operator such as leading negation, `box`, `rc`, etc.
        /// Flags: none
        Unary(AstExprUnaryOp),

        /// A binary operator.
        /// Flags: none
        BinOp(AstExprBinOp),

        /// A string literal with sub-expressions for templating.
        /// Flags: `cont`
        StrLit(AstExprStrLit),

        /// A number literal.
        /// Flags: `cont`
        NumLit(AstExprNumLit),

        /// A block scope. This is potentially confusable with the scopes of an `if/while <cond> <scope>`
        /// expression so allowing expression atomization to finish after a `cont` is necessary to
        /// disambiguate.
        /// Flags: `cont`, `no_prev_cont`
        Scope(AstExprScope),

        /// A parenthetical expression. This is potentially confusable with function call syntax
        /// and must be distinguished based off `cont` context.
        /// Flags: `cont`, `no_prev_cont`
        Paren(AstExprParen),

        /// A direct object constructor. May overlap with `Fetch` which is why this must be matched
        /// with a higher priority with a disambiguating lookahead.
        /// Flags: `cont`
        Ctor(AstExprConstructor),

        /// A closure definition.
        /// Flags: `cont`
        Closure(AstExprClosure),

        /// An if(-else) block
        /// Flags: `cont`
        If(AstExprIf),

        /// A while block
        /// Flags: `cont`
        While(AstExprWhile),

        /// A loop
        /// Flags: `cont`
        Loop(AstExprLoop),

        /// A return ~~statement~~ err I mean expression.
        /// This is an expression because people might want to write `=> return foo;` in a match
        /// expression so we just treat it as an expression returning `!`.
        /// Flags: none
        Return(AstExprReturn),

        /// A break expression. See `Return` for rationale.
        /// Flags: none
        Break(AstExprBreak),

        /// A continue expression. See `Return` for rationale.
        /// Flags: none
        Continue(AstExprContinue),

        /// A yield expression. See `Return` for rationale.
        /// Flags: none
        Yield(AstExprYield),

        /// A cast expression. This has the highest precedence of all other "operators" so it's fine
        /// to match this during atomization and doing so avoids having to re-parse `Fields` as types
        /// during folding.
        /// Flags: `requires_cont`. This is not `cont`.
        Cast(AstExprCast),
    }
}

impl AstExpr {
    // TODO: We should really add support for `enum_meta` and `enum_categories` simultaneously.
    pub fn is_cont(&self) -> bool {
        match self {
            AstExpr::Fetch(_) => true,
            AstExpr::Index(_) => true,
            AstExpr::Field(_) => true,
            AstExpr::Call(_) => true,
            AstExpr::Todo(_) => false,
            AstExpr::Unary(_) => false,
            AstExpr::BinOp(_) => false,
            AstExpr::StrLit(_) => true,
            AstExpr::NumLit(_) => true,
            AstExpr::Scope(_) => true,
            AstExpr::Paren(_) => true,
            AstExpr::Ctor(_) => true,
            AstExpr::Closure(_) => true,
            AstExpr::If(_) => true,
            AstExpr::While(_) => true,
            AstExpr::Loop(_) => true,
            AstExpr::Return(_) => false,
            AstExpr::Break(_) => false,
            AstExpr::Continue(_) => false,
            AstExpr::Yield(_) => false,
            AstExpr::Cast(_) => false,
        }
    }
}

impl AstExpr {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        Some(Self::parse_maybe(reader)??)
    }

    pub fn parse_maybe(reader: &mut TokenStreamReader) -> Option<Option<Self>> {
        reader.lookahead(|reader| {
            // Stage 1: Atomize
            // In the first stage of expression parsing, we transform the token stream into a stream
            // of unambiguous "atoms" and ambiguous binary/unary operators.
            let mut atoms = Vec::<AstExpr>::new();
            loop {
                let atom = match_choice!(
                    &mut *reader,
                    // Match ctor.
                    // See variant doc-comment for details.
                    [|reader| Some(AstExprConstructor::parse(reader)?.wrap())],
                    // Match everything else.
                    [
                        //> Match non-contextual atoms
                        // Fetch
                        |reader| Some(AstExprFetch::parse(reader)?.wrap()),
                        // To-do
                        |reader| Some(AstExprTodo::parse(reader)?.wrap()),
                        // Unary
                        |reader| Some(AstExprUnaryOp::parse(reader)?.wrap()),
                        // Binary
                        |reader| Some(AstExprBinOp::parse(reader)?.wrap()),
                        // Str-lit
                        |reader| Some(AstExprStrLit::parse(reader)?.wrap()),
                        // Num-lit
                        |reader| Some(AstExprNumLit::parse(reader)?.wrap()),
                        // Closure
                        |reader| Some(AstExprClosure::parse(reader)?.wrap()),
                        // If(-else)
                        |reader| Some(AstExprIf::parse(reader)?.wrap()),
                        // While
                        |reader| Some(AstExprWhile::parse(reader)?.wrap()),
                        // Loop
                        |reader| Some(AstExprLoop::parse(reader)?.wrap()),
                        // Return
                        |reader| Some(AstExprReturn::parse(reader)?.wrap()),
                        // Break
                        |reader| Some(AstExprBreak::parse(reader)?.wrap()),
                        // Continue
                        |reader| Some(AstExprContinue::parse(reader)?.wrap()),
                        // Yield
                        |reader| Some(AstExprYield::parse(reader)?.wrap()),
                        //> Match continuation atoms (e.g. field accesses, indexers)
                        |reader| {
                            // Ensure that we're following a cont
                            atoms.last().filter(|expr| expr.is_cont())?;

                            // Match atoms
                            match_choice!(
                                reader,
                                // Index
                                |reader| Some(AstExprIndex::parse(reader)?.wrap()),
                                // Field
                                |reader| Some(AstExprField::parse(reader)?.wrap()),
                                // Call
                                |reader| Some(AstExprCall::parse(reader)?.wrap()),
                                // Cast
                                |reader| Some(AstExprCast::parse(reader)?.wrap()),
                            )
                        },
                        // > Match non-continuation atoms (e.g. paren)
                        |reader| {
                            // Ensure that we're following a cont
                            match atoms.last() {
                                Some(atom) if atom.is_cont() => return None,
                                _ => {}
                            }

                            // Match atoms
                            match_choice!(
                                reader,
                                // Scope
                                |reader| Some(AstExprScope::parse(reader)?.wrap()),
                                // Paren
                                |reader| Some(AstExprParen::parse(reader)?.wrap()),
                            )
                        }
                    ]
                );

                if let Some(atom) = atom {
                    atoms.push(atom);
                } else {
                    // The stream can stop here.
                    break;
                }
            }

            // Stage 2: Fold
            // To transform this into an expression tree, we fold the atom list over several folding
            // passes. These passes correspond to the order-of-operations hierarchy.

            // TODO
            // assert!(atoms.len() <= 1);

            // Finalize
            Some(atoms.into_iter().next())
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprFetch {
    pub path: AstPathDirect,
}

impl AstExprFetch {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        Some(Self {
            path: AstPathDirect::parse(reader)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprIndex {
    pub left: Option<Rc<AstExpr>>,
    pub index: Rc<AstExpr>,
}

impl AstExprIndex {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let group = util_match_group_delimited(reader, GroupDelimiter::Bracket)?;
            let index = Rc::new(AstExpr::parse(&mut group.reader())?);
            Some(Self { left: None, index })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprField {
    pub left: Option<Rc<AstExpr>>,
    pub name: String,
}

impl AstExprField {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_punct(reader, PunctChar::Period, None)?;
            let name = util_match_ident(reader)?;

            Some(Self {
                left: None,
                name: name.text(),
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprCall {
    pub left: Option<Rc<AstExpr>>,
    pub args: Vec<AstExpr>,
}

impl AstExprCall {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match paren
            let group = util_match_group_delimited(reader, GroupDelimiter::Paren)?;

            // Consume expression argument list
            let mut reader = group.reader();
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));
            let args = reader
                .consume_while(|reader| {
                    delimited.next(reader)?;
                    Some(AstExpr::parse(reader))
                })
                .try_collect()
                .ok()?;

            // Match EOF
            util_match_eof(&mut reader)?;

            // Produce expression
            Some(Self { left: None, args })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprTodo;

impl AstExprTodo {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        util_match_ellipsis(reader).and(Some(AstExprTodo))
    }
}

#[derive(Debug, Clone)]
pub struct AstExprUnaryOp {
    pub op: UnaryOp,
    pub target: Option<Rc<AstExpr>>,
}

impl AstExprUnaryOp {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        let op = TokenLitSeq::match_list(
            reader,
            UnaryOp::values_iter().map(|(op, meta)| (meta.char, op)),
        )?;

        Some(Self { op, target: None })
    }
}

enum_meta! {
    #[derive(Debug)]
    pub enum(UnaryOpMeta) UnaryOp {
        Neg = UnaryOpMeta {
            char: TokenLitSeq::Punct(&[PunctChar::Dash]),
            right: false,
        },
        Try = UnaryOpMeta {
            char: TokenLitSeq::Punct(&[PunctChar::Question]),
            right: true,
        },
        Box = UnaryOpMeta {
            char: TokenLitSeq::Kw(AstKeyword::Box),
            right: false,
        },
        Rc = UnaryOpMeta {
            char: TokenLitSeq::Kw(AstKeyword::Rc),
            right: false,
        },
        Gc = UnaryOpMeta {
            char: TokenLitSeq::Kw(AstKeyword::Gc),
            right: false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct UnaryOpMeta {
    char: TokenLitSeq<'static>,
    right: bool,
}

#[derive(Debug, Clone)]
pub struct AstExprBinOp {
    pub op: BinOp,
    pub left: Option<Rc<AstExpr>>,
    pub right: Option<Rc<AstExpr>>,
}

impl AstExprBinOp {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        let op = TokenLitSeq::match_list(reader, BinOp::values_iter().map(|(op, seq)| (*seq, op)))?;

        Some(Self {
            op,
            left: None,
            right: None,
        })
    }
}

enum_meta! {
    #[derive(Debug)]
    pub enum(TokenLitSeq<'static>) BinOp {
        Add = TokenLitSeq::Punct(&[PunctChar::Plus]),
        Sub = TokenLitSeq::Punct(&[PunctChar::Dash]),
        Div = TokenLitSeq::Punct(&[PunctChar::Slash]),
        Mul = TokenLitSeq::Punct(&[PunctChar::Asterisk]),
        And = TokenLitSeq::Punct(&[PunctChar::Ampersand]),
        Or = TokenLitSeq::Punct(&[PunctChar::Bar]),
        Xor = TokenLitSeq::Punct(&[PunctChar::Caret]),
        ShortAnd = TokenLitSeq::Punct(&[PunctChar::Ampersand, PunctChar::Ampersand]),
        ShortOr = TokenLitSeq::Punct(&[PunctChar::Bar, PunctChar::Bar]),
    }
}

#[derive(Debug, Clone)]
pub struct AstExprStrLit {
    pub parts: Vec<AstExprStrPart>,
}

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstExprStrPart {
        Literal(String),
        Template(AstScope),
    }
}

impl AstExprStrLit {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let lit = util_match_str_lit(reader)?;
            let parts = lit
                .parts
                .iter()
                .map(|part| match part {
                    StringComponent::Template(group) => {
                        Some(AstScope::parse_inner(&mut group.reader())?.wrap())
                    }
                    StringComponent::Literal(lit) => Some(lit.clone().wrap()),
                })
                .try_collect()
                .ok()?;

            Some(Self { parts })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprNumLit {
    pub lit: TokenNumberLit,
}

impl AstExprNumLit {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        util_match_num_lit(reader).map(|lit| Self { lit: lit.clone() })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprScope {
    pub label: Option<String>,
    pub scope: AstScope,
}

impl AstExprScope {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let label = if util_match_punct(reader, PunctChar::Ampersand, None).is_some() {
                Some(util_match_ident(reader)?.text())
            } else {
                None
            };
            let scope = AstScope::parse_braced(reader)?;

            Some(Self { label, scope })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprParen {
    pub expr: Rc<AstExpr>,
}

impl AstExprParen {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let paren = util_match_group_delimited(reader, GroupDelimiter::Paren)?;
            let mut reader = paren.reader();
            let expr = AstExpr::parse(&mut reader)?;
            util_match_eof(&mut reader)?;
            Some(Self {
                expr: Rc::new(expr),
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprConstructor {
    pub ty: AstType,
    pub ctor: AstExprCtorKind,
}

impl AstExprConstructor {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            let ty = AstType::parse(reader)?;
            let ctor = match_choice!(
                reader,
                // Match tuple
                |reader| Some(AstExprTupleCtor::parse(reader)?.wrap()),
                // Match struct
                |reader| Some(AstExprStructCtor::parse(reader)?.wrap()),
            )?;

            Some(Self { ty, ctor })
        })
    }
}

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstExprCtorKind {
        Tuple(AstExprTupleCtor),
        Struct(AstExprStructCtor),
    }
}

#[derive(Debug, Clone)]
pub struct AstExprTupleCtor {
    pub exprs: Vec<AstExpr>,
}

impl AstExprTupleCtor {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match tuple paren group
            let paren = util_match_group_delimited(reader, GroupDelimiter::Paren)?;
            let mut reader = paren.reader();

            // Collect the delimited list
            let mut delimited = DelimiterMatcher::new_start(util_punct_matcher(PunctChar::Comma));

            let exprs = reader
                .consume_while(|reader| {
                    delimited.next(reader)?;
                    AstExpr::parse(reader)
                })
                .collect();

            // Match EOF
            util_match_eof(&mut reader)?;

            Some(Self { exprs })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprStructCtor {
    pub fields: Vec<(String, AstExpr)>,
    pub default_spread: bool,
}

impl AstExprStructCtor {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            // Match struct brace group
            let brace = util_match_group_delimited(reader, GroupDelimiter::Brace)?;
            let mut reader = brace.reader();

            // Collect the initializer list
            let punct = util_punct_matcher(PunctChar::Comma);
            let mut delimited = DelimiterMatcher::new_start(&punct);
            let fields = reader
                .consume_while(|reader| {
                    delimited.lookahead(|delimited| {
                        delimited.next(reader);

                        let name = util_match_ident(reader)?;
                        util_match_punct(reader, PunctChar::Colon, None)?;
                        let expr = AstExpr::parse(reader)?;

                        Some((name.text(), expr))
                    })
                })
                .collect();

            // Match optional trailing comma
            delimited.next(&mut reader);

            // Match optional default spread
            let default_spread = util_match_ellipsis(&mut reader).is_some();

            // Match brace EOF
            util_match_eof(&mut reader)?;

            Some(Self {
                fields,
                default_spread,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprClosure {
    def: AstFuncDef,
}

impl AstExprClosure {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        Some(Self {
            // TODO: Parsing a normal func-def is insufficient. We need a flag to disable names and prototypes.
            def: AstFuncDef::parse(reader)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprIf {
    pub cond: Rc<AstExpr>,
    pub if_case: AstScope,
    pub else_if: Vec<(AstExpr, AstScope)>,
    pub else_case: Option<AstScope>,
}

impl AstExprIf {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        reader.lookahead(|reader| {
            util_match_specific_kw(reader, AstKeyword::If)?;
            let cond = AstExpr::parse(reader)?;
            let if_case = AstScope::parse_braced(reader)?;

            let else_if = reader
                .consume_while(|reader| {
                    util_match_specific_kw(reader, AstKeyword::Else)?;
                    util_match_specific_kw(reader, AstKeyword::If)?;

                    let cond = AstExpr::parse(reader)?;
                    let case = AstScope::parse_braced(reader)?;
                    Some((cond, case))
                })
                .collect();

            let else_case = if util_match_specific_kw(reader, AstKeyword::Else).is_some() {
                Some(AstScope::parse_braced(reader)?)
            } else {
                None
            };

            Some(Self {
                cond: Rc::new(cond),
                if_case,
                else_if,
                else_case,
            })
        })
    }
}

#[derive(Debug, Clone)]
pub struct AstExprWhile {}

impl AstExprWhile {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprLoop {}

impl AstExprLoop {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprReturn {}

impl AstExprReturn {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprBreak {}

impl AstExprBreak {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprContinue {}

impl AstExprContinue {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprYield {}

impl AstExprYield {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}

#[derive(Debug, Clone)]
pub struct AstExprCast {}

impl AstExprCast {
    pub fn parse(reader: &mut TokenStreamReader) -> Option<Self> {
        None // TODO
    }
}
