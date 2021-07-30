(** Quickcheck generator of sexps that obey a grammar. *)

open! Base

val create : _ Sexp_grammar.t -> Sexp.t Base_quickcheck.Generator.t
