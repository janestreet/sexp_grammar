## Release v0.16.0

* Depends on Core rather than Base.

* Refactored representation of recursive types. Potentially-recursive definitions are now
  stored with each `Tycon` constructor. Within the definitions, the `Recursive`
  constructor allows references by name only. This fixes some bugs with substitution of
  type definitions, and allows incremental unrolling of recursive types.

* Added `unroll_tycon` and `unroll_tycon_untyped` for unrolling type definitions at the
  top level of a grammar.

* Added deriving `sexp` instead of just `sexp_of` for grammar types.

* Moved `validate_sexp*` from `Sexp_grammar_validation` to `Sexp_grammar`.

* Stopped requiring `quickcheck_observer` for `Sexp_grammar_validation.validate_grammar*`.
