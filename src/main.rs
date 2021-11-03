#![allow(dead_code)]
#![feature(allocator_api)]
#![feature(coerce_unsized)]
#![feature(decl_macro)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]
#![feature(str_internals)]
#![feature(unsize)]

use crate::syntax::parse::module::AstModule;
use crate::syntax::span::SourceFile;
use crate::syntax::token::tokenize_file;
use std::path::PathBuf;

pub mod syntax;
pub mod util;

fn main() {
    env_logger::init();

    let source = SourceFile::from_file(PathBuf::from("examples/syn/class.crew")).unwrap();
    let tokens = tokenize_file(&mut source.reader());
    println!("Tokenized source: {}", tokens);

    let parsed = AstModule::parse(&mut tokens.reader());
    println!("Parsed source: {:#?}", parsed);
}
