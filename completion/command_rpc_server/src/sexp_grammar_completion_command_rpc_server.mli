(** Provides a default implementation for the [Sexp_grammar_completion_protocol].

    Sexp grammar devs will update this library as the protocol evolves, and smash the tree
    for any interface changes that entails. *)

open! Core
open! Async
open! Import

(** [create grammar ~summary] returns a command to start a [Command_rpc] server
    implementing the [Sexp_grammar_protocol]. [summary] is passed through to [Command]. *)
val create : _ Sexp_grammar.t -> summary:string -> Command.t
