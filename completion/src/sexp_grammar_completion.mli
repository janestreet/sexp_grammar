(** [Sexp_grammar_completion] implements type-directed tab-completion of sexps. *)

open! Core
open! Import
open Sexp_grammar_completion_protocol

(** Returns a staged function which generates suggestion candidates from a grammar. *)
val complete : _ Sexp_grammar.t -> (Prefix.t -> Candidates.t Or_error.t) Staged.t

(** Creates a [Command.Auto_complete.t] function from a sexp grammar for CLI completions.
    Does not return completions that would be inserted to the right of the point (i.e.
    closing parens). Filters completion results to those consistent with the prefix. *)
val command_auto_complete : _ Sexp_grammar.t -> Command.Auto_complete.t Staged.t
