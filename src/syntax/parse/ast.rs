// === Modules === //

use crate::syntax::token::{PunctChar, TokenGroup, TokenIdent, TokenNumberLit};
use crate::util::enum_utils::{enum_categories, enum_meta};

#[derive(Debug, Clone)]
pub struct AstMod {
    pub inner_attrs: Vec<AstAttr>,
    pub defs: Vec<AstDef>,
}

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstDef {
        Func(AstFnDef),
        Class(AstClassDef),
        Enum(AstEnumDef),
        Mod(AstModDef),
        Use(AstUseDef),
        Macro(AstMacroDef),
        Extern(AstExternDef),
        Type(AstTypeDef),
    }
}

#[derive(Debug, Clone)]
pub struct AstFnDef {
    pub vis: AstVis,
    pub name: TokenIdent,
    pub inner: AstFnDefInner,
}

#[derive(Debug, Clone)]
pub struct AstFnDefInner {
    pub attrs: Vec<AstAttr>,
    // ...
}

#[derive(Debug, Clone)]
pub struct AstClassDef {
    pub vis: AstVis,
    pub attrs: Vec<AstAttr>,
}

#[derive(Debug, Clone)]
pub struct AstEnumDef {
    pub vis: AstVis,
    pub attrs: Vec<AstAttr>,
}

#[derive(Debug, Clone)]
pub struct AstModDef {
    pub vis: AstVis,
    pub attrs: Vec<AstAttr>,
}

#[derive(Debug, Clone)]
pub struct AstUseDef {
    pub vis: AstVis,
    pub attrs: Vec<AstAttr>,
}

#[derive(Debug, Clone)]
pub struct AstMacroDef {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExternDef {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstTypeDef {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstAttr {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstVis {
    // ...
}

// === Types === //

#[derive(Debug, Clone)]
pub struct AstType {}

// === Expressions === //

enum_categories! {
    #[derive(Debug, Clone)]
    pub enum AstExpr {
        // === Misc === //
        Path(AstPath),
        Paren(AstParen),
        MacroExp(AstMacroCall),
        Closure(AstClosureDef),
        Block(AstBlock),

        // === Literals === //
        NumberLit(AstNumberLit),
        StringLit(AstStringLit),
        TupleLit(AstTupleLit),
        ArrayLit(AstArrayLit),

        // === Operators === //
        Cast(AstExprCastOp),
        BinOp(AstExprBinOp),
        UnaryOp(AstExprUnaryOp),
        SuffixOp(AstExprSuffixOp),

        // === Accessors === //
        Index(AstExprIndex),
        Property(AstExprProperty),
        Call(AstExprCall),

        // === Control flow === //
        If(AstExprIf),
        While(AstExprWhile),
        Loop(AstExprLoop),
        Match(AstExprMatch),
        Break(AstExprBreak),
        Continue(AstExprContinue),
        Return(AstExprReturn),
    }
}

#[derive(Debug, Clone)]
pub struct AstPath {
    pub is_absolute: bool,
    pub parts: Vec<TokenIdent>,
}

#[derive(Debug, Clone)]
pub struct AstParen {
    pub inner: Box<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstMacroCall {
    pub path: AstPath,
    pub args: TokenGroup,
}

#[derive(Debug, Clone)]
pub struct AstClosureDef {
    pub inner: AstFnDefInner,
}

#[derive(Debug, Clone)]
pub struct AstNumberLit {
    pub lit: TokenNumberLit,
}

#[derive(Debug, Clone)]
pub struct AstStringLit {
    pub comps: Vec<AstStringComp>,
}

#[derive(Debug, Clone)]
pub enum AstStringComp {
    Lit(String),
    Template(AstExpr),
}

#[derive(Debug, Clone)]
pub struct AstTupleLit {
    pub elems: Vec<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstArrayLit {
    pub elems: Vec<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstExprCastOp {
    pub target: Box<AstExpr>,
    pub kind: AstCastKind,
    pub ty: AstType,
}

#[derive(Debug, Clone)]
pub struct AstExprBinOp {
    pub left: Box<AstExpr>,
    pub op: AstBinOpKind,
    pub right: Box<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstExprUnaryOp {
    pub op: AstUnaryOpKind,
    pub right: Box<AstExpr>,
}

#[derive(Debug, Clone)]
pub struct AstExprSuffixOp {
    pub left: Box<AstExpr>,
    pub op: AstSuffixOpKind,
}

#[derive(Debug, Clone)]
pub struct AstExprIndex {}

#[derive(Debug, Clone)]
pub struct AstExprProperty {}

#[derive(Debug, Clone)]
pub struct AstExprCall {}

#[derive(Debug, Clone)]
pub struct AstBlock {
    pub statements: Vec<AstBlockStatement>,
    pub last_expr: Box<AstExpr>,
}

#[derive(Debug, Clone)]
pub enum AstBlockStatement {
    Def(AstDef),
    Expr(AstExpr),
}

#[derive(Debug, Clone)]
pub struct AstExprIf {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprWhile {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprLoop {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprMatch {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprBreak {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprContinue {
    // ...
}

#[derive(Debug, Clone)]
pub struct AstExprReturn {
    // ...
}

// === Categories === //

enum_meta! {
    #[derive(Debug)]
    pub enum(&'static str) AstKeyword {
        Pub = "pub",
        Crate = "crate",
        Class = "class",
        Enum = "enum",
        Fn = "fn",
        Static = "static",
        Macro = "macro",
        Use = "use",
        Mod = "mod",
        If = "if",
        Loop = "loop",
        While = "while",
        Match = "match",
        Break = "break",
        Continue = "continue",
        Return = "return",
        Open = "open",
        In = "in",
        Out = "out",
        Impl = "impl",
        Readonly = "readonly",
        Val = "val",
        Var = "var",

        // === Reserved === //
        Abstract = "abstract",
        Final = "final",
        Box = "box",
        Gc = "gc",
        Rc = "rc",
    }

    #[derive(Debug)]
    pub enum(&'static [PunctChar]) AstBinOpKind {}

    #[derive(Debug)]
    pub enum(&'static [PunctChar]) AstUnaryOpKind {}

    #[derive(Debug)]
    pub enum(&'static [PunctChar]) AstSuffixOpKind {}

    #[derive(Debug)]
    pub enum(Option<PunctChar>) AstCastKind {
        /// Static casts must be provable at compile-time
        Static = None,

        /// Infallible casts will panic at runtime.
        Infailable = Some(PunctChar::Exclamation),

        /// Fallible casts will return `None` at runtime.
        Fallible = Some(PunctChar::Question),
    }
}
