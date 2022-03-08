//! # Stage 1 - Tokenizer
//!
//! This module contains both the token tree IR and the producer logic for the compiler's
//! tokenization stage.
//!
//! This stage accomplishes two important tasks. First, it groups the source file into balanced
//! token streams--making opaque group consumption (e.g. as done by macros or un-parsable groups) much
//! easier to implement--while providing the flexibility necessary for users to define custom
//! syntactical elements. Second, it abstracts over the complexities of Unicode by providing atomic
//! abstractions (e.g. idents, puncts, strings, comments) over them.
//!
//! In addition to the benefits of its IR, the tokenizer also provides:
//!
//! - Early interning of identifiers and literals
//! - Unbalanced group recovery
//! - Invalid character recovery
//! - Literal parsing
//! - Doc comment lowering
//! - Shebang ignoring
//!
//! The entry point for this stage is the [tokenize::tokenize_file] function.

pub mod ir;
pub mod tokenize;
