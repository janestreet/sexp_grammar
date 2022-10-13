open! Core

module Grammar : sig
  module V1 : sig
    type t [@@deriving sexp_grammar]

    include Stable with type t := t

    val to_grammar : t -> Sexplib0.Sexp_grammar.grammar
    val of_grammar : Sexplib0.Sexp_grammar.grammar -> t
  end

  module V2 : sig
    type t [@@deriving sexp_grammar]

    include Stable with type t := t

    val to_grammar : t -> Sexplib0.Sexp_grammar.grammar
    val of_grammar : Sexplib0.Sexp_grammar.grammar -> t
  end

  module V3 : sig
    type t = Sexplib0.Sexp_grammar.grammar [@@deriving sexp_grammar]

    include Stable with type t := t
  end

end
