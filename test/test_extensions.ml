open! Core
open Sexp_grammar_validation

let%expect_test "expression w/ wildcard" =
  validate_grammar
    (module struct
      type t = Sexp.t [@@deriving quickcheck, sexp]

      (* make sure this works without annotation *)
      let t_sexp_grammar = [%sexp_grammar: _]

      (* make sure type extension works *)
      let (t_sexp_grammar : [%sexp_grammar: _]) = t_sexp_grammar
    end)
  |> ok_exn;
  [%expect {| (Any _) |}]
;;

let%expect_test "expression w/ variant attribute" =
  validate_grammar
    (module struct
      type t =
        [ `A
        | `B of [ `C ] list [@sexp.list]
        ]
      [@@deriving quickcheck, sexp]

      (* make sure this works without annotation *)
      let t_sexp_grammar = [%sexp_grammar: [ `A | `B of [ `C ] list [@sexp.list] ]]

      (* make sure type extension works *)
      let (t_sexp_grammar : [%sexp_grammar: [ `A | `B of [ `C ] list ]]) = t_sexp_grammar
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Any_case)
      (clauses
       (((name A) (clause_kind Atom_clause))
        ((name B)
         (clause_kind
          (List_clause
           (args
            (Many
             (Variant
              ((name_kind Any_case)
               (clauses (((name C) (clause_kind Atom_clause))))))))))))))) |}]
;;
