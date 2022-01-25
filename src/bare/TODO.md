# To-Do

## Nitpicks

- [x] `TokenStreamReaders` should maintain an enclosing span so that `next_loc` and `prev_loc` always return something
- [ ] A lot of parser code can be modernized with new idioms.
- [ ] The bump allocator could be reference-counted per page to allow for more precise memory deallocation, especially during visitor remaps.
- [ ] We can unify `try_collect` and `collect_into` with the notion of an `ArrayVec`.
- [ ] `TokenStreams` should also have a slice type.
- [ ] `Folders` should support the closure-return system.
- [x] The `Folder` API should support relative and absolute positions.
- [ ] Multi-lookahead can be made more ergonomic.
- [ ] We should implement a task system with async multithreaded execution.
- [ ] We should really use an interner.

## Tokenizer

- [x] Implement public tokenizer IR interface
- [x] Implement token/span printing
- [ ] Improve string literal handling:
    - [x] Multiline strings
    - [x] Unicode escapes
    - [ ] Character literals
    - [ ] Multiline indent bars
    - [ ] Custom string delimiters
- [ ] Raw identifiers (e.g. `#out`)
- [x] Finish numeric literal parsing (how do we handle property accesses and ranges)
- [ ] Preserve doc and delimiter comments
- [ ] Reconsider character groups (we might need to look into preventing certain Unicode exploits)
- [x] Implement basic diagnostics
- [ ] Implement advanced diagnostics/error recovery system
- [ ] Test suite

## Parser

- [x] Implement simple "production rules" style grammar.
- [ ] Implement diagnostic.
- [ ] Implement error recovery.
- [ ] Track source spans for diagnostics later on.
- [ ] Test suite

## ATP

- [ ] Define grammar
- [ ] Implement theorem prover
- [ ] Implement inference system

## Semantic

- [ ] Lexical resolution
- [ ] Proposition validation
- [ ] Expression validation
