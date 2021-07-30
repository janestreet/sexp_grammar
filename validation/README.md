# Sexp_grammar_validation validates grammars against t_of_sexp

`Sexp_grammar_validation` checks that a grammar actually describes the
behavior of the associated `t_of_sexp` function. This is a
precondition for all uses of sexp grammars.

#### What defines a valid grammar?

A grammar is valid if it accepts all sexps that `t_of_sexp` can parse.

Note that a grammar need not reject all unparseable sexps. `t_of_sexp`
may enforce non-syntactic invariants. E.g., `Map.t_of_sexp` requires
that the keys be unique.

A logician might say that sexp grammars are complete but not sound. We
will avoid this sense of "complete" because sexp grammars are commonly
used for tab-completion.

#### Why might a grammar be invalid?

1. It is possible to confuse the grammars of two different types, for
   example if the grammar is polymorphic, or via misuse of `coerce`.

2. Custom sexp conversions require custom grammars. One might write
   the custom grammar incorrectly, or forget to write it at all.

#### How does this library validate a grammar?

We use quickcheck to search for parseable sexps that the grammar
rejects. Specifically,

1. We generate values of `t` whose sexps can be parsed by `t_of_sexp`,
   and assert that the grammar accepts those sexps.

2. We generate sexps resembling but not matching the grammar, and
   assert that `t_of_sexp` rejects those sexps.
