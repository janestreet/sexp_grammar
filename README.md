Sexp_grammar
============

The `Sexp_grammar` library provides accessors for manipulating the
types defined in `Sexplib0.Sexp_grammar`. Values of these types are
often generated via `[@@deriving sexp_grammar]`. For documentation of
the types themselves, see `sexp_grammar.ml` in `sexplib0`. For
documentation of the deriving syntax, see
[`ppx_sexp_conv`](%{root}/ppx/ppx_sexp_conv/README.org#deriving-sexp_grammar).

There are also supporting libraries under this directory.
`Sexp_grammar_validation` provides tools for testing that a sexp
grammar matches an `_of_sexp` parser, and `Sexp_grammar_completion`
(not currently publicly released) provides a facility for
autocompletion based on sexp grammars.
