#![allow(dead_code)]
#![feature(decl_macro)]
#![feature(str_internals)]

use crate::syntax::intern::Interner;
use crate::syntax::span::SourceFile;
use crate::syntax::token::tokenize_file;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    let mut interner = Interner::new();
    let source = SourceFile::from_file(PathBuf::from("examples/simple_groups.crew")).unwrap();
    println!("{:#?}", tokenize_file(&mut interner, &source));
}
