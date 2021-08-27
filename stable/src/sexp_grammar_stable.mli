open! Core

module Grammar : sig
  module V1 : sig
    include Stable

    val to_grammar : t -> Sexplib0.Sexp_grammar.grammar
    val of_grammar : Sexplib0.Sexp_grammar.grammar -> t
  end

  module V2 : sig
    include Stable

    val to_grammar : t -> Sexplib0.Sexp_grammar.grammar
    val of_grammar : Sexplib0.Sexp_grammar.grammar -> t
  end

  module V3 : Stable with type t = Sexplib0.Sexp_grammar.grammar

end
