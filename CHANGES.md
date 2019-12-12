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