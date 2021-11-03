# To-Do

## Infrastructure

- [x] Trace logging
- [ ] Efficient data structures:
  - [x] ~~Bump allocator~~ (`bumaplo` is sufficiently powerful when using the allocator API)
  - [ ] Fork `std` collections
  - [ ] Interner
- [x] Macro to automate object category generation
- [ ] Create in-place array folding mechanisms:
  - [x] Base interface
  - [x] Advanced folding mechanisms
  - [ ] Left folder
- [ ] Diagnostic system
- [ ] Crash reporter
- [ ] Task system

### Nitpicks

- [ ] Enum meta should support variants
- [ ] Object categories should be nestable
- [ ] Enum meta and object categories should support generics
- [ ] `Readers` should support mutable semantics
- [ ] We should make some more `Reader` adapters
- [ ] `Folders` should support the closure-return system
- [ ] The `Folder` API can be improved significantly.
- [ ] Ideally, we could optimize trees by representing them as state machines.
- [ ] We create a lot of temporaries unnecessarily.
- [ ] Parsing loads the entire file into memory
- [ ] Diagnostics should be allowed to use the tokenized source and the AST to produce properly colored code samples.
- [ ] `TokenStreamReaders` should maintain an enclosing span so that `next_loc` and `prev_loc` always return something.
- [ ] `TokenStreams` should also have a slice type.

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

- [ ] Define expression AST
- [ ] Implement expression parsing
- [ ] Define scopes and their parsing
- [ ] Define modules and their parsing
- [ ] Implement diagnostic and error recovery system
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
- [ ] Garbage collection with support for finalizers, cleanup hooks, and weak references.
- [ ] User-defined value types through `struct`.
- [ ] Combine Java-style enum metadata with Rust-style sum type enums
- [ ] Actual `statics`
- [ ] Programmatic `TokenStream` macros (support macros by example?).
- [ ] Attribute macros which can operate on a typed IR
- [ ] Apply qualifiers to blocks, allow qualifiers to span several lines
- [ ] Type aliases qualified with `in` as an alternative to generics.
- [ ] Remote qualifiers using the `<qualifier> on <item>` syntax. Mostly used for `impl`.
- [ ] Class/tuple deconstruction
- [ ] Operator overloading (differentiate between assign and copy)
- [ ] Reflection
- [ ] Class properties
- [ ] Programmatic dynamic exposition (`IDynamicObject` interface)
- [ ] Allow users to dynamically specify input fields (as either values or property getters) and methods (as closures).
- [ ] Unified `async` and iterator handling through coroutines.
- [ ] No required runtime, allow host to decide which function is the entry point.
- [ ] `static` is handled by creating an associated singleton class instance. The static singleton is accessed by the class' name. In a class context, `static` annotates arbitrary items to mark them as going in the singleton instead of in the class instance. The only functional difference between `static` items and normal items is that fields must either have an initialization routine or the class must define a static initializer block. This means that static classes can expose other classes including `IDefault`, an interface which allows users to skip that field while directly instantiating a class.
