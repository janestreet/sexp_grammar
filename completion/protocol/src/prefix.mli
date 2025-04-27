(** [Prefix.t] describes a path through an incomplete sexp up to but not including the
    current atom, if the point is in an incomplete atom.

    Not included in [Prefix.t]:

    1. preceding complete toplevel sexps, because we consider each sexp independently.
    2. the atom prefix, because we leave it to the client to filter by atom prefix.

    (See {!Rpc_complete} for more on the division of labor between client and server.) *)

open! Core
open! Import

type t =
  | Hole
  | In_list of Sexp.t list * t
[@@deriving compare, sexp_of]

include Comparator.S with type t := t

(** [of_sexp_prefix sexp_prefix] discards preceding complete toplevel sexps. Any atom
    prefix is returned separately. *)
val of_sexp_prefix
  :  Parsexp_prefix.Sexp_prefix.t
  -> t * Parsexp_prefix.Atom_prefix.t option

(** [of_substring s ~pos ~len] returns [None] if the specified substring is not a valid
    [Sexp_prefix.t]. *)
val of_substring
  :  string
  -> pos:int
  -> len:int
  -> (t * Parsexp_prefix.Atom_prefix.t option) option

module Unstable : sig
  type nonrec t = t [@@deriving sexp]
end

module Stable : sig
  module V1 : Stable with type t = t with type comparator_witness = comparator_witness
end
