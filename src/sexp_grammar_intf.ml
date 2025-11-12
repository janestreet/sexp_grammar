open! Core

[@@@warning "-30"] (*_ allow duplicate field names *)

open struct
  module Sexp_grammar = Sexplib0.Sexp_grammar
end

module Field = struct
  type 'a t =
    | Optional of 'a
    | Required of 'a
end

module And_tags = struct
  type 'a t = 'a * (string, Sexp.t) List.Assoc.t
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

  val record
    :  (string, list_t Field.t And_tags.t) List.Assoc.t
    -> allow_extra_fields:bool
    -> list_t

  val variant
    :  (string, list_t option And_tags.t) List.Assoc.t
    -> case_sensitivity:Sexp_grammar.case_sensitivity
    -> t

  val lazy_ : t Lazy.t -> t
  val tag : t -> string -> Sexp.t -> t
end

(** Callbacks for nonrecursive folds. *)
module type Callbacks_for_fold_nonrecursive = sig
  include Callbacks_for_fold_common

  (** Callers must explicitly handle constructors for recursive grammars. *)

  val tyvar : string -> t
  val tycon : string -> params:t list -> defns:(string, string list * t) List.Assoc.t -> t
  val recursive : string -> params:t list -> t
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

module type Sexp_grammar = sig @@ portable
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

  module Case_sensitivity : sig
    type t = Sexp_grammar.case_sensitivity =
      | Case_insensitive
      | Case_sensitive
      | Case_sensitive_except_first_character
    [@@deriving sexp_of]

    (** Produces a comparator that compares with respect to a name kind. *)
    val to_string_comparator : t -> (module Comparator.S with type t = string)
  end

  type grammar : value mod contended portable = Sexp_grammar.grammar =
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
    | Tagged of grammar with_tag
    | Tyvar of string
    | Tycon of string * grammar list * defn list
    | Recursive of string * grammar list
    | Lazy of grammar Portable_lazy.t
  [@@unsafe_allow_any_mode_crossing]

  and list_grammar = Sexp_grammar.list_grammar =
    | Empty
    | Cons of grammar * list_grammar
    | Many of grammar
    | Fields of record

  and record = Sexp_grammar.record =
    { allow_extra_fields : bool
    ; fields : field with_tag_list list
    }

  and field = Sexp_grammar.field =
    { name : string
    ; required : bool
    ; args : list_grammar
    }

  and case_sensitivity = Sexp_grammar.case_sensitivity =
    | Case_insensitive
    | Case_sensitive
    | Case_sensitive_except_first_character

  and variant : value mod contended portable = Sexp_grammar.variant =
    { case_sensitivity : case_sensitivity
    ; clauses : clause with_tag_list list
    }
  [@@unsafe_allow_any_mode_crossing]

  and clause = Sexp_grammar.clause =
    { name : string
    ; clause_kind : clause_kind
    }

  and clause_kind = Sexp_grammar.clause_kind =
    | Atom_clause
    | List_clause of { args : list_grammar }

  and 'a with_tag = 'a Sexp_grammar.with_tag =
    { key : string
    ; value : Sexp.t
    ; grammar : 'a
    }

  and 'a with_tag_list = 'a Sexp_grammar.with_tag_list =
    | Tag of 'a with_tag_list with_tag
    | No_tag of 'a

  and defn = Sexp_grammar.defn =
    { tycon : string
    ; tyvars : string list
    ; grammar : grammar
    }

  and 'a t = 'a Sexp_grammar.t = { untyped : grammar }
  [@@unboxed] [@@deriving bin_io, compare ~localize, equal ~localize, sexp]

  (** For stable serializations of these types, see [Sexp_grammar_stable]. *)

  val coerce : _ t -> _ t @@ portable

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

  (** {2 Tagging} *)

  (** [first_tag_value tags name of_sexp] returns the first value of [name] in [tags]. *)
  val first_tag_value
    :  (string * Sexp.t) list
    -> string
    -> [%of_sexp: 'a]
    -> 'a Or_error.t option

  (** [completion_suggested = false] on a variant constructor means that
      [Sexp_grammar_completion] will not suggest the constructor as a completion. The
      constructor is still recognized as valid syntax. Completions are still suggested for
      its arguments.

      Default is [true].

      This tag is ignored if its value is not a bool or if it is not placed on a variant
      constructor. *)
  val completion_suggested : string

  (** [validate_sexp [%sexp_grammar: t]] prepares a function to report whether the grammar
      of [t] accepts a sexp.

      Staged because the outer application does a lot of work. It is often valuable to
      apply [accepts] to a grammar once, then apply the result to multiple sexps. *)
  val validate_sexp : _ t -> (Sexp.t -> unit Or_error.t) Staged.t

  (** [validate_sexp_untyped] is like [validate_sexp] but takes the untyped grammar. *)
  val validate_sexp_untyped : grammar -> (Sexp.t -> unit Or_error.t) Staged.t

  (** [validate_sexp_list] is like [validate_sexp] but validates a sequence of sexps. *)
  val validate_sexp_list : list_grammar -> (Sexp.t list -> unit Or_error.t) Staged.t

  (** [unroll_tycon [%sexp_grammar: t]] returns an equivalent grammar in which the
      top-most node is not a [Tycon].

      Acts as identity if the condition is already satisfied, and does a shallow
      evaluation of the [Tycon] otherwise.

      If [tag_prefix] is provided, then [Recursive] and [Tyvar] nodes substituted by
      [unroll_tycon] will be tagged respectively with keys [tag_prefix ^ ".tycon"] and
      [tag_prefix ^ ".tyvar"]. The value is the name of the tycon / tyvar. *)
  val unroll_tycon : ?tag_prefix:string -> 'a t -> 'a t

  (** [unroll_tycon_untyped] is like [unroll_tycon] but takes the untyped grammar. *)
  val unroll_tycon_untyped : ?tag_prefix:string -> grammar -> grammar

  (** [known_to_accept_all_sexps [%sexp_grammar: t]] returns [true] if it is statically
      known that all sexps satisfy the grammar. In practice, such grammars are [Any], or
      (possibly nested) [Union]s where one of the branches is an [Any].

      This check is not complete, i.e. it will return [false] on some grammars which *do*
      accept all sexps. *)
  val known_to_accept_all_sexps : 'a t -> bool
end
