#![allow(dead_code)]
#![feature(allocator_api)]
#![feature(build_hasher_simple_hash_one)]
#![feature(coerce_unsized)]
#![feature(decl_macro)]
#![feature(maybe_uninit_array_assume_init)]
#![feature(maybe_uninit_extra)]
#![feature(maybe_uninit_uninit_array)]
#![feature(str_internals)]
#![feature(unsize)]
#![feature(int_log)]

use crate::syntax::parse::module::AstModule;
use crate::syntax::span::{ReadAtom, SourceFile};
use crate::syntax::token::tokenize_file;
use crate::util::reader::StreamReader;
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
