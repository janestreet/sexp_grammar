open! Core
open! Import

let%expect_test _ =
  let open struct
    [@@@warning "-37"]

    type t =
      | Malformed_tag [@tag Sexp_grammar.completion_suggested = Atom "not a bool"]
      | Should_not_suggest of bool
      [@tag Sexp_grammar.completion_suggested = [%sexp false]]
      | Should_suggest of bool
      | Should_suggest_explicitly of bool
      [@tag Sexp_grammar.completion_suggested = [%sexp true]]
    [@@deriving sexp_grammar]
  end in
  let complete = unstage (Sexp_grammar_completion.complete t_sexp_grammar) in
  (* Don't suggest [Should_not_suggest]. Do suggest the others, including [Malformed_tag] *)
  show complete "";
  [%expect
    {|
    │
    =>
    (inexhaustive)
    Malformed_tag│                (matches atom prefix)
    (Should_suggest │)            (matches atom prefix)
    (Should_suggest_explicitly │) (matches atom prefix)
    |}];
  (* But if they type out [Should_not_suggest] themselves, help them with the arguments *)
  show complete "(Should_not_suggest ";
  [%expect
    {|
    (Should_not_suggest │
    =>
    (Should_not_suggest false│ (matches atom prefix)
    (Should_not_suggest true│  (matches atom prefix)
    |}]
;;
