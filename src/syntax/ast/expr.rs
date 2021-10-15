use crate::syntax::span::Span;
use crate::util::enum_utils::enum_meta;

#[derive(Debug, Clone)]
pub struct AstExpr {
    span: Span,
    side_effects: Vec<()>,
}

enum_meta! {
    #[derive(Debug)]
    pub enum(&'static str) AstKeyword {
        Class = "class",
        In = "in",
        Out = "out",
        Self_ = "self",
        If = "if",
        Else = "else",
        Loop = "loop",
        While = "while",
    }
}
