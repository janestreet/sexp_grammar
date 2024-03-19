open! Core
open Expect_test_helpers_core

open struct
  type binary_tree =
    | Leaf
    | Node of binary_tree * binary_tree
  [@@deriving sexp, sexp_grammar]

  let example_grammars =
    [ [%sexp_grammar: unit].untyped
    ; [%sexp_grammar: bool].untyped
    ; [%sexp_grammar: (int, float) Either.t list option].untyped
    ; [%sexp_grammar: binary_tree].untyped
    ; [%sexp_grammar: (unit[@tag "key" = Atom "value"])].untyped
    ]
  ;;
end

module _ : module type of struct
  include Sexp_grammar_stable
end [@ocaml.remove_aliases] = struct
  module Grammar = struct
    module V4 = struct
      open struct
        module T = Sexp_grammar_stable.Grammar.V4
      end

      include (
        T : Stable with type t = T.t with type comparator_witness = T.comparator_witness)

      let t_sexp_grammar = T.t_sexp_grammar

      let%expect_test _ =
        print_and_check_stable_type [%here] (module T) example_grammars;
        [%expect
          {|
          (bin_shape_digest 2b208c8f56151b7eaa218daab3ea0a58)
          ((sexp (List Empty)) (bin_io "\007\000"))
          ((sexp   Bool)
           (bin_io "\001"))
          ((sexp (
             Option (
               List (
                 Many (
                   Variant (
                     (case_sensitivity Case_sensitive_except_first_character)
                     (clauses (
                       (No_tag (
                         (name First)
                         (clause_kind (List_clause (args (Cons Integer Empty))))))
                       (No_tag (
                         (name Second)
                         (clause_kind (List_clause (args (Cons Float Empty))))))))))))))
           (bin_io
            "\006\007\002\b\002\002\001\005First\001\001\003\000\001\006Second\001\001\004\000"))
          ((sexp (
             Lazy (
               Tycon binary_tree
               ()
               ((
                 (tycon binary_tree)
                 (tyvars ())
                 (grammar (
                   Variant (
                     (case_sensitivity Case_sensitive_except_first_character)
                     (clauses (
                       (No_tag (
                         (name        Leaf)
                         (clause_kind Atom_clause)))
                       (No_tag (
                         (name Node)
                         (clause_kind (
                           List_clause (
                             args (
                               Cons
                               (Recursive binary_tree ())
                               (Cons (Recursive binary_tree ()) Empty)))))))))))))))))
           (bin_io
            "\014\012\011binary_tree\000\001\011binary_tree\000\b\002\002\001\004Leaf\000\001\004Node\001\001\r\011binary_tree\000\001\r\011binary_tree\000\000"))
          ((sexp (
             Tagged (
               (key   key)
               (value value)
               (grammar (List Empty)))))
           (bin_io "\n\003key\000\005value\007\000"))
          |}]
      ;;
    end
  end
end
