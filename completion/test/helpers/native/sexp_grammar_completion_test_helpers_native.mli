open! Core

(** [backtesting_command grammar ~name] returns a group of commands for backtesting
    completions against a corpus of known-good files matching [grammar].

    When such a corpus exists, automated backtesting on each roll is strongly recommended
    as a complement to the [quickcheck] support in
    {{!Sexp_grammar_completion_test_helpers} [Sexp_grammar_completion_test_helpers]}. *)
val backtesting_command : _ Sexp_grammar.t -> name:string -> Command.t
