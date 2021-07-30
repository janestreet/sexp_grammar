open! Base

(** Reports whether the grammar accepts the given sexp. Staged because the outer
    application does a lot of work. It is often valuable to apply [accepts] to a grammar
    once, then apply the result to multiple sexps. *)

val accepts : _ Sexp_grammar.t -> (Sexp.t -> bool) Staged.t
val grammar_accepts : Sexp_grammar.grammar -> (Sexp.t -> bool) Staged.t
val list_grammar_accepts : Sexp_grammar.list_grammar -> (Sexp.t list -> bool) Staged.t

(** Like [accepts], but gives an error message instead of [false]. *)

val validate : _ Sexp_grammar.t -> (Sexp.t -> unit Or_error.t) Staged.t
val validate_grammar : Sexp_grammar.grammar -> (Sexp.t -> unit Or_error.t) Staged.t

val validate_list_grammar
  :  Sexp_grammar.list_grammar
  -> (Sexp.t list -> unit Or_error.t) Staged.t
