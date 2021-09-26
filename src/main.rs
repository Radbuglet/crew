#![allow(dead_code)]
#![feature(decl_macro)]
#![feature(str_internals)]

use crate::syntax::intern::Interner;
use crate::syntax::span::{AsFileReader, SourceFile};
use crate::syntax::token::group_match_number_lit;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    let mut interner = Interner::new();
    let source = SourceFile::from_file(PathBuf::from("examples/number.crew")).unwrap();
    group_match_number_lit(&mut source.reader(), &mut interner);
}
