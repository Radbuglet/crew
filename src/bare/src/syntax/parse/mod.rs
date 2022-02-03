//! # Stage 2 - AST Parsing
//!
//! This module contains code for the compiler's abstract syntax tree (AST) parsing stage.
//!
//! This stage converts available token streams into a grammatically validated tree representation.
//! No identifiers are linked and program semantics are almost completely unchecked.
//!
//! The entrypoint for this module is the [module::AstModule::parse] method.

pub mod macros;
pub mod module;
pub mod path;
pub mod ty;
pub mod util;
