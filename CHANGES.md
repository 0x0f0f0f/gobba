## 0.3.1 2019-12-26
### Added
- `map2` primitive
- Reading programs from files
- An abstraction that allows treating primitives as functions, and so allows
  currying and all the fancy stuff.
- Examples in the `examples/` directory
- *Greater or equal to* and *Less than or equal to* operators
- More test coverage.
### Bugfixes
- Fixed various bugs of 0.3

## 0.3 2019-12-25
### Added
- A simple AST optimizer
- Multiple let assignments (`let x = 2 and y = 3 and z = ... in ...`)
- Function currying (partial application)
- Strings
- Sequences of expressions separated with `;` are treated as a single expression
  that returns the value of the last one.
- Function pipelining (reverse composition) with the `>=>` operator.
  (`(fun x y -> x + y >=> g) 1 2` is `g 3`)
- Dictionaries (`{"hello": 123, "world": 345}`)
- Map, foldl and filter on lists and dictionaries
- Testing and code coverage using alcotest and bisect_ppx
### Bugfixes
- Fixed function application parser

## 0.2.2 2019-12-13
### Added
- `let lazy rec` (or `let rec lazy`) to define recursive lazy functions
- Command line option parser and man page
- Command line option `-v` (`--verbose`) to tell the REPL to show the AST and
  reduction steps.
- Evaluation step printing is now done in correct order and nesting. 
### Bugfixes
- Fixed boolean primitives.
### Removed
- `lazyfun` and `lazylambda` statements.

## 0.2.1 2019-12-12
### Added
- Optional `lazy` evaluation! You can use `let lazy` to define values that will
  be evaluated only when encountered in the `let lazy` expression body.
- Lazy anonymous functions! Just like the `let lazy` statement you can now use
  `lazyfun` or `lazylambda` to define anonymous functions which arguments will
  be lazily evaluated.
### Bugfixes
- Fixed precedence levels in parser
### Roadmap
- Cache for lazy evaluation
- Partial application

## 0.2 2019-12-08
### Added
- Lists
- Unit

## 0.1 2019-12-03
### Added
- Initial release of the project