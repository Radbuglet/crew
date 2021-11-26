# To-Do

## Infrastructure

- [x] Trace logging
- [ ] Efficient data structures:
  - [ ] Bump allocator
  - [ ] Fork `std` collections
  - [ ] Interner
  - [ ] Contiguous allocation optimization
- [x] Macro to automate object category generation
- [ ] Create in-place array folding mechanisms:
  - [x] Base interface
  - [x] Advanced folding mechanisms
  - [ ] Left folder
- [ ] Diagnostic system
- [ ] Crash reporter
- [ ] Task system

### Nitpicks

- [x] `TokenStreamReaders` should maintain an enclosing span so that `next_loc` and `prev_loc` always return something
- [ ] `TokenStreams` should also have a slice type
- [ ] `Folders` should support the closure-return system
- [ ] The `Folder` API can be improved significantly
- [ ] Parsing loads the entire file into memory
- [ ] Diagnostics should be allowed to use the tokenized source and the AST to produce properly highlighted code samples

## Tokenizer

- [x] Implement public tokenizer IR interface
- [x] Implement token/span printing
- [ ] Improve string literal handling:
  - [x] Multiline strings 
  - [ ] Unicode escapes
  - [ ] Single characters
  - [ ] Multiline indent bars
  - [ ] Custom delimiters
- [ ] Finish numeric literal parsing (how do we handle property accesses and ranges)
- [ ] Preserve doc and delimiter comments
- [ ] Preserve insignificant characters (e.g. whitespace, comments) for formatting.
- [ ] Revisit handling of invalid newlines/characters
- [ ] Reconsider character groups
- [ ] Implement error recovery system
- [ ] Test suite

## Parser

- [ ] Implement simple "production rules" style grammar.
- [ ] Generalize parser traits.
- [ ] Implement diagnostic.
- [ ] Implement error recovery.
- [ ] Optimize matching system
- [ ] Track source spans for diagnostics later on.
- [ ] Test suite

# Feature Wishlist

- [x] Multiline strings with templating
- [x] Nested comments
- [ ] `//>` region comments
- [ ] Doc comments
- [ ] Unified enum and type matching system through `is`
- [ ] Linking `in` at runtime if they are statically unbound (should allow late binding with checks)
- [ ] Allow users to specify the scope of who can provide values for a given `in`.
- [ ] `in out` linking inference
- [ ] Error propagation (`?`) syntax with implicit conversion
- [ ] `return` to named block
- [ ] `break`/`continue`/`return` within specific closures that support it (done by propagating a `ControlFlow<T, L>` result object)
- [ ] Garbage collection with support for finalizers, cleanup hooks, and weak references. GC pointers are just smart pointers in userland, with deref handled through typical exposition.
- [ ] Also support `RCs` and `Boxes` in userland using the exposed allocator API. Allocators can be passed as object inputs.
- [ ] Combine Java-style enum metadata with Rust-style sum type enums
- [ ] Actual `statics`
- [ ] Programmatic `TokenStream` macros (support macros by example?).
- [ ] Attribute macros which can operate on a typed IR
- [x] Apply qualifiers to blocks, allow qualifiers to span several lines
- [x] Type aliases qualified with `in` as an alternative to generics.
- [x] Remote qualifiers using the `<qualifier> on <item>` syntax. Mostly used for `impl`.
- [ ] Class/tuple deconstruction
- [ ] Operator overloading (differentiate between assign and copy)
- [ ] Reflection
- [ ] Class properties
- [ ] Programmatic dynamic exposition (`IDynamicObject` interface)
- [ ] Allow users to dynamically specify input fields (as either values or property getters) and methods (as closures).
- [ ] Unified `async` and iterator handling through coroutines.
- [ ] No required runtime, allow host to decide which function is the entry point.
- [ ] `static` is handled by creating an associated singleton class instance. The static singleton is accessed by the class' name. In a class context, `static` annotates arbitrary items to mark them as going in the singleton instead of in the class instance. The only functional difference between `static` items and normal items is that fields must either have an initialization routine or the class must define a static initializer block. This means that static classes can expose other classes including `IDefault`, an interface which allows users to skip that field while directly instantiating a class.
- [ ] Support `on path.to._` syntax.
- [ ] Support move and copy semantics with user-defined `Drop` handlers. Objects with unqualified `in` are unsized and stored as VLAs. When taking ownership of a nested component which depends on its parents, cast the entire allocation to that unsized component.
- [ ] Inline classes (basically Java's inline implementation of abstract classes feature)
- [ ] Struct types (essentially tuples with named fields).
- [ ] Full relational type syntax. (e.g. `:>`, `:<`, `=`, and `:` as an alias for `:<`)
- [ ] Function expression syntax.
