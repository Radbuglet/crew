#![allow(dead_code)]
#![feature(decl_macro)]
#![feature(str_internals)]

use crate::syntax::span::SourceFile;
use crate::syntax::token::tokenize_file;
use crate::util::intern::Interner;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    env_logger::init();

    let mut interner = Interner::new();
    let source = SourceFile::from_file(PathBuf::from("examples/tokenize.crew")).unwrap();
    let _ = tokenize_file(&mut interner, &source);
}
