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
    module V1 = struct
      open struct
        module T = Sexp_grammar_stable.Grammar.V1
      end

      include (
        T : Stable with type t = T.t with type comparator_witness = T.comparator_witness)

      let to_grammar = T.to_grammar
      let of_grammar = T.of_grammar

      let%expect_test _ =
        print_and_check_stable_type
          [%here]
          (module T)
          (List.map ~f:T.of_grammar example_grammars);
        [%expect
          {|
          (bin_shape_digest 84c03b5f322e69fd22373ed914009e60)
          ((sexp (List Empty)) (bin_io "\b\000"))
          ((sexp   Bool)
           (bin_io "\001"))
          ((sexp (
             Option (
               List (
                 Many (
                   Variant (
                     (name_kind Capitalized)
                     (clauses (
                       ((name First)  (args (Cons Integer Empty)))
                       ((name Second) (args (Cons Float   Empty)))))))))))
           (bin_io "\007\b\002\t\001\002\005First\001\003\000\006Second\001\004\000"))
          ((sexp (
             Lazy (
               Recursive
               (Tycon binary_tree ())
               ((
                 (tycon binary_tree)
                 (tyvars ())
                 (grammar (
                   Union (
                     (Enum ((name_kind Capitalized) (names (Leaf))))
                     (Variant (
                       (name_kind Capitalized)
                       (clauses ((
                         (name Node)
                         (args (
                           Cons
                           (Tycon binary_tree ())
                           (Cons (Tycon binary_tree ()) Empty))))))))))))))))
           (bin_io
            "\014\r\012\011binary_tree\000\001\011binary_tree\000\n\002\006\001\001\004Leaf\t\001\001\004Node\001\012\011binary_tree\000\001\012\011binary_tree\000\000"))
          ((sexp (List Empty)) (bin_io "\b\000")) |}]
      ;;
    end

    module V2 = struct
      open struct
        module T = Sexp_grammar_stable.Grammar.V2
      end

      include (
        T : Stable with type t = T.t with type comparator_witness = T.comparator_witness)

      let to_grammar = T.to_grammar
      let of_grammar = T.of_grammar

      let%expect_test _ =
        print_and_check_stable_type
          [%here]
          (module T)
          (List.map ~f:T.of_grammar example_grammars);
        [%expect
          {|
          (bin_shape_digest 351d08efb322ce8b9cf8124e41f3d248)
          ((sexp (List Empty)) (bin_io "\007\000"))
          ((sexp   Bool)
           (bin_io "\001"))
          ((sexp (
             Option (
               List (
                 Many (
                   Variant (
                     (name_kind Capitalized)
                     (clauses (
                       ((name First)
                        (clause_kind (List_clause (args (Cons Integer Empty)))))
                       ((name Second)
                        (clause_kind (List_clause (args (Cons Float Empty)))))))))))))
           (bin_io
            "\006\007\002\b\001\002\005First\001\001\003\000\006Second\001\001\004\000"))
          ((sexp (
             Lazy (
               Recursive
               (Tycon binary_tree ())
               ((
                 (tycon binary_tree)
                 (tyvars ())
                 (grammar (
                   Variant (
                     (name_kind Capitalized)
                     (clauses (
                       ((name        Leaf)
                        (clause_kind Atom_clause))
                       ((name Node)
                        (clause_kind (
                          List_clause (
                            args (
                              Cons
                              (Tycon binary_tree ())
                              (Cons (Tycon binary_tree ()) Empty))))))))))))))))
           (bin_io
            "\r\012\011\011binary_tree\000\001\011binary_tree\000\b\001\002\004Leaf\000\004Node\001\001\011\011binary_tree\000\001\011\011binary_tree\000\000"))
          ((sexp (List Empty)) (bin_io "\007\000")) |}]
      ;;
    end

    module V3 = struct
      open struct
        module T = Sexp_grammar_stable.Grammar.V3
      end

      include (
        T : Stable with type t = T.t with type comparator_witness = T.comparator_witness)

      let%expect_test _ =
        print_and_check_stable_type [%here] (module T) example_grammars;
        [%expect
          {|
          (bin_shape_digest 441ef514ccf19b6bfaaf348b2074bc2c)
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
               Recursive
               (Tycon binary_tree ())
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
                               (Tycon binary_tree ())
                               (Cons (Tycon binary_tree ()) Empty)))))))))))))))))
           (bin_io
            "\014\r\012\011binary_tree\000\001\011binary_tree\000\b\002\002\001\004Leaf\000\001\004Node\001\001\012\011binary_tree\000\001\012\011binary_tree\000\000"))
          ((sexp (
             Tagged (
               (key   key)
               (value value)
               (grammar (List Empty)))))
           (bin_io "\n\003key\000\005value\007\000")) |}]
      ;;
    end
  end
end
