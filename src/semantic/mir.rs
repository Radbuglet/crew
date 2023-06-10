// === Type === //

#[derive(Debug, Clone)]
pub struct MirType<'a> {
    base: MirTypeBase<'a>,
    params: &'a [MirType<'a>],
}

#[derive(Debug, Clone)]
pub enum MirTypeBase<'a> {
    // Special
    Generic { id: &'a MirGeneric<'a> },

    // Userland
    Struct { def: &'a MirStruct<'a> },

    // Pointers
    WideRef,
    ThinRef,
    Pointer,

    // Numbers
    Bool,
    Integer { width: u16, signed: bool },
    Float32,
    Float64,
}

#[derive(Debug, Clone)]
pub struct MirGeneric<'a> {
    name: &'a str,
}

#[derive(Debug, Clone)]
pub struct MirStruct<'a> {
    name: &'a str,
}

// === Function === //

#[derive(Debug, Clone)]
pub struct MirFunction<'a> {
    generics: &'a [&'a MirGeneric<'a>],
    arguments: &'a [&'a MirVariable<'a>],
    imp: MirFunctionImpl<'a>,
}

#[derive(Debug, Clone)]
pub enum MirFunctionImpl<'a> {
    Userland {
        locals: &'a [&'a MirVariable<'a>],
        entry: &'a MirBb<'a>,
        bbs: &'a [&'a MirBb<'a>],
    },
    Intrinsic(MirIntrinsicFn),
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub enum MirIntrinsicFn {
    Print,
}

#[derive(Debug, Clone)]
pub struct MirVariable<'a> {
    name: &'a str,
    ty: &'a MirType<'a>,
}

#[derive(Debug, Clone)]
pub struct MirBb<'a> {
    stmts: &'a [MirStmt<'a>],
}

#[derive(Debug, Clone)]
pub enum MirStmt<'a> {
    Call { target: &'a MirFunction<'a> },
}
