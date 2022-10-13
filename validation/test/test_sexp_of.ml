open! Base
open! Import

let test grammar = print_s [%sexp (grammar : _ Sexp_grammar.t)]

let%expect_test "[Int.t_sexp_grammar]" =
  test Int.t_sexp_grammar;
  [%expect {| Integer |}]
;;

let%expect_test "[int_sexp_grammar]" =
  test int_sexp_grammar;
  [%expect {| Integer |}]
;;

let%expect_test "[%sexp_grammar: int]" =
  test [%sexp_grammar: int];
  [%expect {| Integer |}]
;;

let%expect_test "complicated record" =
  let module M = struct
    type t =
      { a : string list
      ; b : int option
      ; c : float Map.M(Bool).t
      }
    [@@deriving sexp_grammar]
  end
  in
  test M.t_sexp_grammar;
  [%expect
    {|
    (List (
      Fields (
        (allow_extra_fields false)
        (fields (
          (No_tag (
            (name     a)
            (required true)
            (args (Cons (List (Many String)) Empty))))
          (No_tag (
            (name     b)
            (required true)
            (args (Cons (Option Integer) Empty))))
          (No_tag (
            (name     c)
            (required true)
            (args (
              Cons
              (Tagged (
                (key sexp_grammar.assoc)
                (value ())
                (grammar (
                  List (
                    Many (
                      List (
                        Cons
                        (Tagged (
                          (key sexp_grammar.assoc.key) (value ()) (grammar Bool)))
                        (Cons
                          (Tagged (
                            (key sexp_grammar.assoc.value)
                            (value ())
                            (grammar Float)))
                          Empty))))))))
              Empty))))))))) |}]
;;
