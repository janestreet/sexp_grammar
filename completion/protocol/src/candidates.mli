(** A [Completion] is the set of suggestions known at a point in a sexp. *)

open! Core
open! Import

type t =
  { candidates : Candidate.t list
  ; exhaustive : bool
  }
[@@deriving compare, fields ~getters, sexp_of]

include Comparator.S with type t := t

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
