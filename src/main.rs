#![allow(dead_code)]
#![feature(allocator_api)]
#![feature(coerce_unsized)]
#![feature(decl_macro)]
#![feature(str_internals)]
#![feature(unsize)]
#![feature(maybe_uninit_uninit_array)]

use crate::syntax::span::SourceFile;
use crate::syntax::token::tokenize_file;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    env_logger::init();

    let source = SourceFile::from_file(PathBuf::from("examples/tokenize.crew")).unwrap();
    let tokens = tokenize_file(&source);
    println!("Tokenized source: {}", tokens);
}
