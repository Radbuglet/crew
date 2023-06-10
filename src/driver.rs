use std::{env, fs};

use bumpalo::Bump;

use crate::{
    syntax::{
        ast::parse_module,
        token::{tokenize, TokenCursor, TokenParser},
    },
    util::{
        misc::UnwrapPrettyExt,
        parser::{StrCursor, StrParser},
    },
};

pub fn entry() {
    let path = env::args().nth(1).expect("missing argument");
    let file = fs::read(path).expect_pretty("failed to read file");
    let file = String::from_utf8(file).expect_pretty("invalid unicode");

    let bump = Bump::new();
    let tokens = tokenize(&bump, &mut StrParser::new(StrCursor::new(&file)))
        .expect_pretty("failed to tokenize");

    let ast = parse_module(
        &bump,
        &mut TokenParser::new(TokenCursor::new(tokens.tokens)),
    )
    .expect_pretty("failed to parse AST");

    dbg!(ast);
}
