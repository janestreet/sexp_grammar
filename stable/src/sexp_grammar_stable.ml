[@@@warning "-30"] (* allow duplicate field names *)

open! Core.Core_stable
module Sexp_grammar = Sexplib0.Sexp_grammar

module V1 = struct
  type grammar =
    | Any of string
    | Bool
    | Char
    | Integer
    | Float
    | String
    | Enum of enum
    | Option of grammar
    | List of list_grammar
    | Variant of variant
    | Union of grammar list
    | Tyvar of string
    | Tycon of string * grammar list
    | Recursive of grammar * defn list
    | Lazy of grammar Lazy.V1.t

  and list_grammar =
    | Empty
    | Cons of grammar * list_grammar
    | Many of grammar
    | Fields of record

  and enum =
    { name_kind : name_kind
    ; names : string list
    }

  and name_kind =
    | Any_case
    | Capitalized

  and record =
    { allow_extra_fields : bool
    ; fields : field list
    }

  and field =
    { name : string
    ; required : bool
    ; args : list_grammar
    }

  and variant =
    { name_kind : name_kind
    ; clauses : clause list
    }

  and clause =
    { name : string
    ; args : list_grammar
    }

  and defn =
    { tycon : string
    ; tyvars : string list
    ; grammar : grammar
    }
  [@@deriving bin_io, compare, sexp, sexp_grammar]

  include struct
    open Base

    let rec to_grammar : grammar -> Sexp_grammar.grammar = function
      | Any name -> Any name
      | Bool -> Bool
      | Char -> Char
      | Integer -> Integer
      | Float -> Float
      | String -> String
      | Enum enum -> Variant (to_atom_variant enum)
      | Option grammar -> Option (to_grammar grammar)
      | List list_grammar -> List (to_list_grammar list_grammar)
      | Variant variant -> Variant (to_list_variant variant)
      | Union grammars -> Union (List.map ~f:to_grammar grammars)
      | Tyvar tyvar_name -> Tyvar tyvar_name
      | Tycon (tycon_name, grammars) -> Tycon (tycon_name, List.map ~f:to_grammar grammars)
      | Recursive (grammar, defns) ->
        Recursive (to_grammar grammar, List.map ~f:to_defn defns)
      | Lazy lazy_grammar -> Lazy (lazy (to_grammar (Lazy.force lazy_grammar)))

    and to_list_grammar = function
      | Empty -> Empty
      | Cons (grammar, list_grammar) ->
        Cons (to_grammar grammar, to_list_grammar list_grammar)
      | Many grammar -> Many (to_grammar grammar)
      | Fields record -> Fields (to_record record)

    and to_record { allow_extra_fields; fields } =
      { allow_extra_fields; fields = List.map ~f:to_field fields }

    and to_field { name; required; args } =
      No_tag { name; required; args = to_list_grammar args }

    and to_atom_variant { name_kind; names } =
      { case_sensitivity = to_case_sensitivity name_kind
      ; clauses = List.map ~f:to_atom_clause names
      }

    and to_atom_clause name = No_tag { name; clause_kind = Atom_clause }

    and to_case_sensitivity : name_kind -> Sexp_grammar.case_sensitivity = function
      | Any_case -> Case_sensitive
      | Capitalized -> Case_sensitive_except_first_character

    and to_list_variant { name_kind; clauses } =
      { case_sensitivity = to_case_sensitivity name_kind
      ; clauses = List.map ~f:to_list_clause clauses
      }

    and to_list_clause { name; args } =
      No_tag { name; clause_kind = List_clause { args = to_list_grammar args } }

    and to_defn { tycon; tyvars; grammar } =
      { tycon; tyvars; grammar = to_grammar grammar }
    ;;

    let rec of_grammar : Sexp_grammar.grammar -> grammar = function
      | Any name -> Any name
      | Bool -> Bool
      | Char -> Char
      | Integer -> Integer
      | Float -> Float
      | String -> String
      | Option grammar -> Option (of_grammar grammar)
      | List list_grammar -> List (of_list_grammar list_grammar)
      | Variant variant -> of_variant variant
      | Union grammars -> Union (List.map ~f:of_grammar grammars)
      | Tyvar tyvar_name -> Tyvar tyvar_name
      | Tycon (tycon_name, grammars) -> Tycon (tycon_name, List.map ~f:of_grammar grammars)
      | Recursive (grammar, defns) ->
        Recursive (of_grammar grammar, List.map ~f:of_defn defns)
      | Lazy lazy_grammar -> Lazy (lazy (of_grammar (Lazy.force lazy_grammar)))
      | Tagged { key = _; value = _; grammar } -> of_grammar grammar

    and of_list_grammar = function
      | Empty -> Empty
      | Cons (grammar, list_grammar) ->
        Cons (of_grammar grammar, of_list_grammar list_grammar)
      | Many grammar -> Many (of_grammar grammar)
      | Fields record -> Fields (of_record record)

    and of_record { allow_extra_fields; fields } =
      { allow_extra_fields; fields = List.map ~f:of_field fields }

    and of_field = function
      | No_tag { name; required; args } -> { name; required; args = of_list_grammar args }
      | Tag { key = _; value = _; grammar } -> of_field grammar

    and of_case_sensitivity : Sexp_grammar.case_sensitivity -> name_kind = function
      | Case_sensitive -> Any_case
      | Case_sensitive_except_first_character -> Capitalized
      | Case_insensitive ->
        Base.Error.raise_s
          [%sexp "Cannot convert a fully case-insensitive grammar to grammar V1."]

    and of_variant { case_sensitivity; clauses } =
      match List.partition_map clauses ~f:of_clause with
      | [], [] -> Union []
      | [], (_ :: _ as clauses) ->
        Variant { name_kind = of_case_sensitivity case_sensitivity; clauses }
      | (_ :: _ as names), [] ->
        Enum { name_kind = of_case_sensitivity case_sensitivity; names }
      | (_ :: _ as names), (_ :: _ as clauses) ->
        Union
          [ Enum { name_kind = of_case_sensitivity case_sensitivity; names }
          ; Variant { name_kind = of_case_sensitivity case_sensitivity; clauses }
          ]

    and of_clause = function
      | No_tag { name; clause_kind } ->
        (match clause_kind with
         | Atom_clause -> First name
         | List_clause { args } -> Second { name; args = of_list_grammar args })
      | Tag { key = _; value = _; grammar } -> of_clause grammar

    and of_defn { tycon; tyvars; grammar } =
      { tycon; tyvars; grammar = of_grammar grammar }
    ;;
  end
end

module V2 = struct
  type grammar =
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
    | Lazy of grammar Lazy.V1.t

  and list_grammar =
    | Empty
    | Cons of grammar * list_grammar
    | Many of grammar
    | Fields of record

  and name_kind =
    | Any_case
    | Capitalized

  and record =
    { allow_extra_fields : bool
    ; fields : field list
    }

  and field =
    { name : string
    ; required : bool
    ; args : list_grammar
    }

  and variant =
    { name_kind : name_kind
    ; clauses : clause list
    }

  and clause =
    { name : string
    ; clause_kind : clause_kind
    }

  and clause_kind =
    | Atom_clause
    | List_clause of { args : list_grammar }

  and defn =
    { tycon : string
    ; tyvars : string list
    ; grammar : grammar
    }
  [@@deriving bin_io, compare, sexp, sexp_grammar]

  include struct
    open Base

    let rec to_grammar : grammar -> Sexp_grammar.grammar = function
      | Any name -> Any name
      | Bool -> Bool
      | Char -> Char
      | Integer -> Integer
      | Float -> Float
      | String -> String
      | Option grammar -> Option (to_grammar grammar)
      | List list_grammar -> List (to_list_grammar list_grammar)
      | Variant variant -> Variant (to_list_variant variant)
      | Union grammars -> Union (List.map ~f:to_grammar grammars)
      | Tyvar tyvar_name -> Tyvar tyvar_name
      | Tycon (tycon_name, grammars) -> Tycon (tycon_name, List.map ~f:to_grammar grammars)
      | Recursive (grammar, defns) ->
        Recursive (to_grammar grammar, List.map ~f:to_defn defns)
      | Lazy lazy_grammar -> Lazy (lazy (to_grammar (Lazy.force lazy_grammar)))

    and to_list_grammar = function
      | Empty -> Empty
      | Cons (grammar, list_grammar) ->
        Cons (to_grammar grammar, to_list_grammar list_grammar)
      | Many grammar -> Many (to_grammar grammar)
      | Fields record -> Fields (to_record record)

    and to_record { allow_extra_fields; fields } =
      { allow_extra_fields; fields = List.map ~f:to_field fields }

    and to_field { name; required; args } =
      No_tag { name; required; args = to_list_grammar args }

    and to_case_sensitivity : name_kind -> Sexp_grammar.case_sensitivity = function
      | Any_case -> Case_sensitive
      | Capitalized -> Case_sensitive_except_first_character

    and to_list_variant { name_kind; clauses } =
      { case_sensitivity = to_case_sensitivity name_kind
      ; clauses = List.map ~f:to_list_clause clauses
      }

    and to_list_clause { name; clause_kind } =
      No_tag { name; clause_kind = to_clause_kind clause_kind }

    and to_clause_kind = function
      | Atom_clause -> Atom_clause
      | List_clause { args } -> List_clause { args = to_list_grammar args }

    and to_defn { tycon; tyvars; grammar } =
      { tycon; tyvars; grammar = to_grammar grammar }
    ;;

    let rec of_grammar : Sexp_grammar.grammar -> grammar = function
      | Any name -> Any name
      | Bool -> Bool
      | Char -> Char
      | Integer -> Integer
      | Float -> Float
      | String -> String
      | Option grammar -> Option (of_grammar grammar)
      | List list_grammar -> List (of_list_grammar list_grammar)
      | Variant variant -> of_variant variant
      | Union grammars -> Union (List.map ~f:of_grammar grammars)
      | Tyvar tyvar_name -> Tyvar tyvar_name
      | Tycon (tycon_name, grammars) -> Tycon (tycon_name, List.map ~f:of_grammar grammars)
      | Recursive (grammar, defns) ->
        Recursive (of_grammar grammar, List.map ~f:of_defn defns)
      | Lazy lazy_grammar -> Lazy (lazy (of_grammar (Lazy.force lazy_grammar)))
      | Tagged { key = _; value = _; grammar } -> of_grammar grammar

    and of_list_grammar = function
      | Empty -> Empty
      | Cons (grammar, list_grammar) ->
        Cons (of_grammar grammar, of_list_grammar list_grammar)
      | Many grammar -> Many (of_grammar grammar)
      | Fields record -> Fields (of_record record)

    and of_record { allow_extra_fields; fields } =
      { allow_extra_fields; fields = List.map ~f:of_field fields }

    and of_field = function
      | No_tag { name; required; args } -> { name; required; args = of_list_grammar args }
      | Tag { key = _; value = _; grammar } -> of_field grammar

    and of_case_sensitivity : Sexp_grammar.case_sensitivity -> name_kind = function
      | Case_sensitive -> Any_case
      | Case_sensitive_except_first_character -> Capitalized
      | Case_insensitive ->
        Base.Error.raise_s
          [%sexp "Cannot convert a fully case-insensitive grammar to grammar V2."]

    and of_variant { case_sensitivity; clauses } =
      Variant
        { name_kind = of_case_sensitivity case_sensitivity
        ; clauses = List.map clauses ~f:of_clause
        }

    and of_clause = function
      | No_tag { name; clause_kind } -> { name; clause_kind = of_clause_kind clause_kind }
      | Tag { key = _; value = _; grammar } -> of_clause grammar

    and of_clause_kind = function
      | Atom_clause -> Atom_clause
      | List_clause { args } -> List_clause { args = of_list_grammar args }

    and of_defn { tycon; tyvars; grammar } =
      { tycon; tyvars; grammar = of_grammar grammar }
    ;;
  end
end

module V3 = struct
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
    | Tagged of grammar with_tag
    | Tyvar of string
    | Tycon of string * grammar list
    | Recursive of grammar * defn list
    | Lazy of grammar Lazy.V1.t

  and list_grammar = Sexp_grammar.list_grammar =
    | Empty
    | Cons of grammar * list_grammar
    | Many of grammar
    | Fields of record

  and case_sensitivity = Sexp_grammar.case_sensitivity =
    | Case_insensitive
    | Case_sensitive
    | Case_sensitive_except_first_character

  and record = Sexp_grammar.record =
    { allow_extra_fields : bool
    ; fields : field with_tag_list list
    }

  and field = Sexp_grammar.field =
    { name : string
    ; required : bool
    ; args : list_grammar
    }

  and variant = Sexp_grammar.variant =
    { case_sensitivity : case_sensitivity
    ; clauses : clause with_tag_list list
    }

  and clause = Sexp_grammar.clause =
    { name : string
    ; clause_kind : clause_kind
    }

  and clause_kind = Sexp_grammar.clause_kind =
    | Atom_clause
    | List_clause of { args : list_grammar }

  and 'a with_tag = 'a Sexp_grammar.with_tag =
    { key : string
    ; value : Sexp.V1.t
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
  [@@deriving bin_io, compare, sexp, sexp_grammar]
end

module Grammar = struct
  module V1 = struct
    type t = V1.grammar [@@deriving bin_io, compare, sexp, sexp_grammar]

    let to_grammar = V1.to_grammar
    let of_grammar = V1.of_grammar

    include (val Comparator.V1.make ~compare ~sexp_of_t)
  end

  module V2 = struct
    type t = V2.grammar [@@deriving bin_io, compare, sexp, sexp_grammar]

    let to_grammar = V2.to_grammar
    let of_grammar = V2.of_grammar

    include (val Comparator.V1.make ~compare ~sexp_of_t)
  end

  module V3 = struct
    type t = V3.grammar [@@deriving bin_io, compare, sexp, sexp_grammar]

    include (val Comparator.V1.make ~compare ~sexp_of_t)
  end
end
