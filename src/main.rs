#![allow(dead_code)]
#![feature(decl_macro)]
#![feature(str_internals)]

use crate::syntax::span::SourceFile;
use crate::syntax::token::tokenize_file;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    let source = SourceFile::from_file(PathBuf::from("examples/parse.crew")).unwrap();
    tokenize_file(&source);
}
