(** Generates sexps that almost but don't actually obey a grammar.

    Simulates a user making mistakes. *)

open! Base

(** Produces a sexp that does not obey the grammar, and an error describing how it fails
    to obey the grammar. *)
val create : _ Sexp_grammar.t -> (Sexp.t * Error.t) Base_quickcheck.Generator.t

module Private : sig
  (** Creates sexps that may or may not obey the grammar. Used to implement [create]. *)
  val create_unfiltered : _ Sexp_grammar.t -> Sexp.t Base_quickcheck.Generator.t
end
