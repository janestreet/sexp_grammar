(** Address of a sub-sexp. *)

open! Base

type t [@@deriving compare, equal, sexp_of]

(** Accessors *)

val get : t -> Sexp.t -> Sexp.t Or_error.t
val set : t -> Sexp.t -> to_:Sexp.t -> Sexp.t Or_error.t
val remove : t -> Sexp.t -> Sexp.t Or_error.t

(** Creators *)

(** [enumerate sexp] returns all possible indices into [sexp] *)
val enumerate : Sexp.t -> t list

val parent : t -> t Or_error.t
