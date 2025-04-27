[@@@warning "-30"] (* allow duplicate field names *)

open! Core.Core_stable
module Sexp_grammar = Sexplib0.Sexp_grammar

module V4 = struct
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
    | Lazy of grammar Portable_lazy.V1.t
  [@@unsafe_allow_any_mode_crossing]

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
  [@@deriving bin_io, compare, equal, sexp, sexp_grammar]
end

module Grammar = struct
  module V4 = struct
    type t = V4.grammar [@@deriving bin_io, compare, equal, sexp, sexp_grammar]

    include (val Comparator.V1.make ~compare ~sexp_of_t)
  end
end
