open! Base

[@@@warning "-30"] (*_ allow duplicate field names *)

open struct
  module Sexp_grammar = Sexplib0.Sexp_grammar
end

module Field = struct
  type 'a t =
    | Optional of 'a
    | Required of 'a
end

(** Common callbacks for folding over a sexp grammar. *)
module type Callbacks_for_fold_common = sig
  (** Represents the result of folding over a grammar. *)
  type t

  (** Represents the result of folding over a list grammar. *)
  type list_t

  (** The remaining functions correspond to grammar constructors. Excludes the
      constructors related to recursive grammars: [Tycon], [Tyvar], and [Recursive]. *)

  val any : string -> t
  val bool : t
  val char : t
  val integer : t
  val float : t
  val string : t
  val option : t -> t
  val union : t list -> t
  val list : list_t -> t
  val empty : list_t
  val cons : t -> list_t -> list_t
  val many : t -> list_t
  val record : (string, list_t Field.t) List.Assoc.t -> allow_extra_fields:bool -> list_t

  val variant
    :  (string, list_t option) List.Assoc.t
    -> name_kind:Sexp_grammar.name_kind
    -> t

  val lazy_ : t Lazy.t -> t
end

(** Callbacks for nonrecursive folds. *)
module type Callbacks_for_fold_nonrecursive = sig
  include Callbacks_for_fold_common

  (** Callers must explicitly handle constructors for recursive grammars. *)

  val tyvar : string -> t
  val tycon : string -> params:t list -> t
  val recursive : t -> defns:(string, string list * t) List.Assoc.t -> t
end

(** Callbacks for recursive folds. *)
module type Callbacks_for_fold_recursive = sig
  include Callbacks_for_fold_common

  (** Allows folds to tie the knot for recursive grammars. Must not force its argument
      immediately. *)
  val of_lazy_recursive : t Lazy.t -> t
end

module type Fold = sig
  type t
  type list_t

  val of_typed_grammar : _ Sexp_grammar.t -> t
  val of_grammar : Sexp_grammar.grammar -> t
  val of_list_grammar : Sexp_grammar.list_grammar -> list_t
end

module type Fold_partial = sig
  type t
  type list_t

  val of_typed_grammar_exn : _ Sexp_grammar.t -> t
  val of_grammar_exn : Sexp_grammar.grammar -> t
  val of_list_grammar_exn : Sexp_grammar.list_grammar -> list_t
end

module type Sexp_grammar = sig
  module type Callbacks_for_fold_common = Callbacks_for_fold_common
  module type Callbacks_for_fold_nonrecursive = Callbacks_for_fold_nonrecursive
  module type Callbacks_for_fold_recursive = Callbacks_for_fold_recursive
  module type Fold = Fold
  module type Fold_partial = Fold_partial

  module Field : sig
    include module type of struct
      include Field
    end

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end

  module Name_kind : sig
    type t = Sexp_grammar.name_kind =
      | Any_case
      | Capitalized
    [@@deriving sexp_of]

    (** Produces a comparator that compares with respect to a name kind. *)
    val to_string_comparator : t -> (module Comparator.S with type t = string)
  end

  type grammar = Sexp_grammar.grammar =
    | Any of string
    | Bool
    | Char
    | Integer
    | Float
    | String
    | Option of grammar
    | List of list_grammar
    | Variant of variant
    | Union of grammar list
    | Tyvar of string
    | Tycon of string * grammar list
    | Recursive of grammar * defn list
    | Lazy of grammar Lazy.t

  and list_grammar = Sexp_grammar.list_grammar =
    | Empty
    | Cons of grammar * list_grammar
    | Many of grammar
    | Fields of record

  and name_kind = Sexp_grammar.name_kind =
    | Any_case
    | Capitalized

  and record = Sexp_grammar.record =
    { allow_extra_fields : bool
    ; fields : field list
    }

  and field = Sexp_grammar.field =
    { name : string
    ; required : bool
    ; args : list_grammar
    }

  and variant = Sexp_grammar.variant =
    { name_kind : name_kind
    ; clauses : clause list
    }

  and clause = Sexp_grammar.clause =
    { name : string
    ; clause_kind : clause_kind
    }

  and clause_kind = Sexp_grammar.clause_kind =
    | Atom_clause
    | List_clause of { args : list_grammar }

  and defn = Sexp_grammar.defn =
    { tycon : string
    ; tyvars : string list
    ; grammar : grammar
    }

  and 'a t = 'a Sexp_grammar.t = { untyped : grammar } [@@unboxed] [@@deriving sexp_of]

  (** For stable serializations of these types, see [Sexp_grammar_stable]. *)

  val coerce : _ t -> _ t

  (** Folds over a grammar. *)
  module Fold_nonrecursive (Callbacks : Callbacks_for_fold_nonrecursive) :
    Fold with type t := Callbacks.t and type list_t := Callbacks.list_t

  (** Like [Fold_nonrecursive]. Ties the knot for recursive grammars. May raise if the
      grammar contains malformed recursive definitions, e.g. an undefined type variable or
      applying a type constructor with the wrong arity. Exceptions may be delayed until
      [of_lazy] values are forced. *)
  module Fold_recursive (Callbacks : Callbacks_for_fold_recursive) :
    Fold_partial with type t := Callbacks.t and type list_t := Callbacks.list_t

  (** An instance of [Fold_nonrecursive]. Produces an equivalent grammar with no [Lazy]
      nodes. Implicitly used by [sexp_of_t]. *)
  module Eager_copy : Fold with type t := grammar and type list_t := list_grammar


  (** An instance of [Fold_recursive]. Produces an equivalent grammar with no [Recursive],
      [Tycon], or [Tyvar] nodes. This can be useful for subsequent grammar processing
      without the need for type variable / type constructor bookkeeping. The resulting
      tree may unfold infinitely, and uses [Lazy] nodes to avoid divergence. *)
  module Unroll_recursion :
    Fold_partial with type t := grammar and type list_t := list_grammar

end
