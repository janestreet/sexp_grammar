open! Core

module type Print_undocumented = sig
  (** [print_undocumented grammar] heuristically searches [grammar] for variant and record
      types, and lists the ones that do not have a doc comment attached to every field or
      constructor.

      By default we list only record fields and variant constructors that are missing doc
      comments. This minimizes churn when people edit doc comments or document their new
      fields (i.e., it makes doing the right thing easy).

      Set [~debug:true] to help identify the types by
      1. listing all fields and constructors, including the ones with doc comments
      2. showing the doc comments *)

  val print_undocumented : ?debug:bool (** default [false] *) -> _ Sexp_grammar.t -> unit
end

module type Sexp_grammar_tools = sig
  include Print_undocumented (** @inline *)
end
