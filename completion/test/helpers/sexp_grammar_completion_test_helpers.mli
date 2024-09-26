(** Helpers for testing completion. *)

open! Core
open! Async
open! Import

val default_mark : string

(** [mark_positions s ~pos] inserts [mark] into [s] at the positions in [pos]. *)
val mark_positions
  :  ?mark:string (** default: [default_mark] *)
  -> string
  -> pos:int list
  -> string

(** [show ?pos complete s] prints the prefix up to [pos] in [s], as it would appear after
    accepting each of the suggestions from [complete]. *)
val show
  :  ?mark:string (** default: [default_mark] *)
  -> ?pos:int (** default: [String.length s] *)
  -> (Sexp_grammar_completion_protocol.Prefix.t
      -> Sexp_grammar_completion_protocol.Candidates.t Or_error.t)
  -> string
  -> unit

(** [show_async] is like [show] but lets [f] return a deferred value. *)
val show_async
  :  ?mark:string (** default: [default_mark] *)
  -> ?pos:int (** default: [String.length s] *)
  -> (Sexp_grammar_completion_protocol.Prefix.t
      -> Sexp_grammar_completion_protocol.Candidates.t Or_error.t Deferred.t)
  -> string
  -> unit Deferred.t

(** [show_sexp grammar s ~pos] prints the completion of [s] at [pos] according to
    [grammar] as a sexp. *)
val show_sexp : _ Sexp_grammar.t -> string -> pos:int -> unit

(** [check_every_position grammar s] completes at every position in [s] and checks that if
    the completions claim to be exhaustive, then at least one of the suggestions is
    consistent with [s].

    Positions with no completion suggestions are shown using [mark_positions]. Separately,
    positions where the completion was inexhaustive and indeed none of the suggestions
    were consistent with [s] are also shown.
*)
val check_every_position : _ Sexp_grammar.t -> string -> unit

(** [backtesting_command grammar ~name] returns a group of commands for backtesting
    completions against a corpus of known-good files matching [grammar].

    When such a corpus exists, automated backtesting on each roll is strongly recommended
    as a complement to the [quickcheck] support below. *)
val backtesting_command : _ Sexp_grammar.t -> name:string -> Command.t

(** [quickcheck grammar sexp_to_string] generates sexps obeying [grammar], serializes them
    using [sexp_to_string], and then checks that exhaustive completions are indeed
    exhaustive.

    [quickcheck] complements backtesting by providing a regression test within the build.
    It is not a replacement. Backtesting is much more effective in exploring the many ways
    a sexp can be serialized to string: comments, whitespace, escaping. Also, backtesting
    does not require the type to implement sexp_of or quickcheck functions. *)
val quickcheck : ?cr:CR.t -> _ Sexp_grammar.t -> (Sexp.t -> string) -> unit