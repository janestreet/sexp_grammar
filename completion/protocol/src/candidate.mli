(** A [Suggestion] describes one possible way to continue a sexp at a position. *)

open! Core
open! Import

module Case_sensitivity : sig
  type t = Sexp_grammar.case_sensitivity =
    | Case_insensitive
    | Case_sensitive
    | Case_sensitive_except_first_character
  [@@deriving enumerate, sexp_of]
end

module Atom_to_add : sig
  type t =
    { atom_signified : string
    (** [atom_signified] is what the atom would be after parsing (e.g., after
        interpretation of escaping and quoting). *)
    ; case_sensitivity : Case_sensitivity.t
    (** Lets a single suggestion match against prefixes of any compatible case. *)
    ; documentation : string list
    (** returns doc comments associated with the grammar at this point *)
    }
  [@@deriving sexp_of]
end

type t =
  | Add_atom of Atom_to_add.t
  | Enter_list
  | Enter_list_and_add_atom of Atom_to_add.t
[@@deriving compare, sexp_of]

include Comparator.S with type t := t

(** To act on a suggestion [t],

    1. delete the current atom, if any,
    2. insert [insert_to_left t] to the left of the point.
    3. insert [insert_to_right t] to the right, leaving the point in between. *)

val insert_to_left : t -> string
val insert_to_right : t -> string

(** [matches_atom_prefix] implements a simple form of that client-side filtering: literal
    prefix matches on the signified atom. See {!Rpc_complete} for more on the division of
    labor between client and server. *)
val matches_atom_prefix : t -> Parsexp_prefix.Atom_prefix.t option -> bool

(** [documentation] returns the documentation provided by the completion server for this
    candidate, if any. Each string is a separate piece of documentation. *)
val documentation : t -> string list

module Unstable : sig
  type nonrec t = t [@@deriving sexp]
end

module Stable : sig
  module V1 : Stable

  module V2 : sig
    include Stable with type t = t with type comparator_witness = comparator_witness

    val to_prev : t -> V1.t
    val from_prev : V1.t -> t
  end
end
