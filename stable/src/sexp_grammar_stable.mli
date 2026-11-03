open! Core

module Grammar : sig
  module V4 : sig
    type t = Sexplib0.Sexp_grammar.grammar [@@deriving equal, sexp_grammar]

    include Stable with type t := t
  end
end
