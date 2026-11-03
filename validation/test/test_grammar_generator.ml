open! Core
open Expect_test_helpers_base

let generator =
  Sexp_grammar_validation.Private.Grammar_generator.quickcheck_generator
    ~gen_recursives:Frequently
;;

module Sexp_grammar = struct
  include Sexp_grammar

  type t = grammar [@@deriving compare ~localize, sexp_of]

  include (val Comparator.make ~compare ~sexp_of_t)
end

let%expect_test "generated grammars include some basic grammars" =
  let total = ref 0 in
  let seen = ref (Set.empty (module Sexp_grammar)) in
  Quickcheck.test generator ~f:(fun grammar ->
    seen := Set.add !seen grammar;
    incr total);
  let num_unique = Set.length !seen in
  print_s [%message (num_unique : int) (!total : int)];
  [%expect
    {|
    ((num_unique 6059)
     (!total     10000))
    |}];
  let equal = Sexp_grammar.equal_grammar in
  List.iter
    [ equal Bool
    ; equal Char
    ; equal Integer
    ; equal Float
    ; equal String
    ; equal (Option Bool)
    ; equal (List Empty)
    ; equal (List (Many Bool))
    ; (function
        | Any _ -> true
        | _ -> false)
    ]
    ~f:(fun f -> Quickcheck.test_can_generate ~f generator);
  [%expect {| |}]
;;

let%expect_test "Distribution looks good" =
  Quickcheck.iter ~trials:10 generator ~f:(fun grammar ->
    print_s [%sexp (grammar : Sexp_grammar.grammar)];
    print_endline "\n=======\n");
  [%expect
    {|
    Char

    =======

    (Tycon 4jyIp
      ((Any "") String Bool)
      (((tycon 4jyIp) (tyvars (E Lta 5)) (grammar (Option Integer)))))

    =======

    (Lazy (
      List (
        Fields (
          (allow_extra_fields false)
          (fields (
            (No_tag (
              (name     qvU76N8b)
              (required true)
              (args (Cons Integer Empty))))
            (No_tag (
              (name     UKZssE)
              (required true)
              (args     Empty)))
            (No_tag (
              (name     zn4X)
              (required false)
              (args     Empty)))
            (No_tag (
              (name     8BxXdhg)
              (required true)
              (args     Empty)))
            (No_tag (
              (name     DQf)
              (required false)
              (args     Empty)))))))))

    =======

    (Option (Option (Any "")))

    =======

    (Variant (
      (case_sensitivity Case_sensitive)
      (clauses ((
        Tag (
          (key SfIe)
          (value ())
          (grammar (
            No_tag (
              (name        MtV5C9w)
              (clause_kind Atom_clause))))))))))

    =======

    (Any "")

    =======

    (Variant (
      (case_sensitivity Case_sensitive_except_first_character)
      (clauses (
        (No_tag (
          (name PRS9I8y) (clause_kind (List_clause (args (Cons Char Empty))))))
        (No_tag (
          (name aDr)
          (clause_kind (
            List_clause (
              args (Cons (List (Cons Bool Empty)) (Cons Integer Empty)))))))
        (No_tag (
          (name        ldnI9wo)
          (clause_kind Atom_clause)))))))

    =======

    (Variant (
      (case_sensitivity Case_sensitive_except_first_character)
      (clauses (
        (No_tag ((name arK2t)  (clause_kind Atom_clause)))
        (No_tag ((name NKVR8n) (clause_kind Atom_clause)))
        (No_tag (
          (name 9GSXV) (clause_kind (List_clause (args (Cons String Empty))))))
        (No_tag (
          (name        jKA)
          (clause_kind Atom_clause)))
        (Tag (
          (key gCs)
          (value ())
          (grammar (
            No_tag (
              (name Kxk)
              (clause_kind (
                List_clause (
                  args (
                    Cons
                    (Tycon NIr0tReA
                      (Char)
                      (((tycon NIr0tReA) (tyvars (O8)) (grammar (Option Char)))
                       ((tycon sNx7Ualb)
                        (tyvars  (ga5 lp))
                        (grammar (Any "")))))
                    (Cons String Empty))))))))))))))

    =======

    String

    =======

    (Option (Any ""))

    =======
    |}]
;;
