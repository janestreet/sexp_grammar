open! Base
open! Import

let%expect_test "[Hash_set.m__t_sexp_grammar]" =
  (module struct
    type t = Core.Hash_set.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok [%here];
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Hashtbl.m__t_sexp_grammar]" =
  (module struct
    type t = int Core.Hashtbl.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok [%here];
  [%expect
    {|
    (Tagged
     ((key sexp_grammar.assoc)
      (value ())
      (grammar
       (List
        (Many
         (List
          (Cons
           (Tagged ((key sexp_grammar.assoc.key) (value ()) (grammar Integer)))
           (Cons
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar Integer)))
            Empty)))))))) |}]
;;

let%expect_test "[Map.m__t_sexp_grammar]" =
  (module struct
    type t = int Core.Map.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok [%here];
  [%expect
    {|
    (Tagged
     ((key sexp_grammar.assoc)
      (value ())
      (grammar
       (List
        (Many
         (List
          (Cons
           (Tagged ((key sexp_grammar.assoc.key) (value ()) (grammar Integer)))
           (Cons
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar Integer)))
            Empty)))))))) |}]
;;

let%expect_test "[Set.m__t_sexp_grammar]" =
  (module struct
    type t = Core.Set.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok [%here];
  [%expect {| (List (Many Integer)) |}]
;;
