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
          ((name     a)
           (required true)
           (args (Cons (List (Many String)) Empty)))
          ((name     b)
           (required true)
           (args (Cons (Option Integer) Empty)))
          ((name     c)
           (required true)
           (args (Cons (List (Many (List (Cons Bool (Cons Float Empty))))) Empty)))))))) |}]
;;
