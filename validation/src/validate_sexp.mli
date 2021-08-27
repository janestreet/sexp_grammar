open! Base

(** [validate_sexp [%sexp_grammar: t]] prepares a function to report whether the grammar
    of [t] accepts a sexp.

    Staged because the outer application does a lot of work. It is often valuable to apply
    [accepts] to a grammar once, then apply the result to multiple sexps. *)
val validate_sexp : _ Sexp_grammar.t -> (Sexp.t -> unit Or_error.t) Staged.t

(** [validate_sexp_untyped] is like [validate_sexp] but takes the untyped grammar. *)
val validate_sexp_untyped : Sexp_grammar.grammar -> (Sexp.t -> unit Or_error.t) Staged.t

(** [validate_sexp_list] is like [validate_sexp] but validates a sequence of sexps. *)
val validate_sexp_list
  :  Sexp_grammar.list_grammar
  -> (Sexp.t list -> unit Or_error.t) Staged.t
