# To-Do

## Tokenizer

- [ ] Improve string literal handling:
  - [ ] Custom delimiters
  - [ ] Single characters
  - [ ] Unicode escapes
  - [ ] Multiline indent bars
- [ ] Finish numeric literal parsing (how do we handle property accesses and ranges)
- [ ] Preserve doc comments
- [ ] Implement public tokenizer IR interface
- [ ] Revisit handling of invalid newlines/characters
- [ ] Implement token/span debug printer
- [ ] Reconsider character groups

## Infrastructure

- [ ] Better backing system
- [ ] More generic interner (`bumpalo`-style allocator with pages?)
- [ ] Task system?
- [ ] Diagnostic system
- [ ] Trace logger
- [ ] Crash reporter

## Parser

- [ ] Create reader
- [ ] Create parsing traits (similar to `syn`)
- [ ] Define expressions
- [ ] Define attributes
- [ ] Define items
- [ ] Define modules

## Semantic

- [ ] Module traversal
- [ ] Identifier linking
- [ ] Visibility pass
- [ ] Dead code warnings (if necessary)
- [ ] Macro expansion (token-level macros pass)
- [ ] Type check/inference & provider validity checks
- [ ] Macro expansion (meta-level macros pass)
- [ ] Closure analysis (capture soundness, cross-closure jump validity and expansion)\
- [ ] Match exhaustiveness validation
- [ ] Control flow analysis
- [ ] Bytecode generation from control flow graph

## Interpreter

- [ ] In-memory representation
- [ ] (de)Serialization
- [ ] Object representation
- [ ] Garbage collector
- [ ] Interpreter
