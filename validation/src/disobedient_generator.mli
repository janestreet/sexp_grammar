(** Generates sexps that almost but don't actually obey a grammar.

    Simulates a user making mistakes. *)

open! Base

(** Produces a sexp that does not obey the grammar, and an error describing how it fails
    to obey the grammar. *)
val create : _ Sexp_grammar.t -> (Sexp.t * Error.t) Base_quickcheck.Generator.t

module Private : sig
  (** Creates sexps that may or may not obey the grammar. Used to implement [create]. *)
  val create_unfiltered : _ Sexp_grammar.t -> Sexp.t Base_quickcheck.Generator.t

  (** Gives up if it's unable to generate a sexp that disobeys the grammar after
      [max_tries] tries. This is needed when testing the grammar generator, because it's
      hard to detect grammars that accept all sexps. *)
  val create_with_max_tries
    :  _ Sexp_grammar.t
    -> max_tries:int
    -> (Sexp.t * Error.t, [ `Max_tries_exceeded ]) Result.t Base_quickcheck.Generator.t
end
