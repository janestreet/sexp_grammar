open! Core
open! Async_kernel
open! Import

(** [backspace atom_prefix] returns the number of bytes to delete to the left of the point
    in order to remove the current atom, if any. *)
val backspace : Atom_prefix.t option -> int

(** [matches candidates prefix string ~pos] returns [true] if at least one of [candidates]
    is consistent with what [string] actually contains at [pos]. Assumes [prefix]
    immediately precedes [pos].

    E.g., [matches [ Left_paren ] None string ~pos = true] if the next token in [string]
    at or after [pos] is a left paren (modulo whitespace, comments, and quotation). *)
val matches : Candidate.t list -> Atom_prefix.t option -> string -> pos:int -> bool
