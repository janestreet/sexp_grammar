open! Base
open! Import

let the_sexp = "((foo 1) (bar ((2 3))) (baz 4))" |> Parsexp.Single.parse_string_exn

let%expect_test "[parent]" =
  let test sexp =
    List.iter (Sexp_index.enumerate sexp) ~f:(fun i ->
      print_s [%sexp (i : Sexp_index.t), (Sexp_index.parent i : Sexp_index.t Or_error.t)])
  in
  test the_sexp;
  [%expect
    {|
    (() (Error ("No parent." ())))
    ((0) (Ok ()))
    ((0 0) (Ok (0)))
    ((0 1) (Ok (0)))
    ((1) (Ok ()))
    ((1 0) (Ok (1)))
    ((1 1) (Ok (1)))
    ((1 1 0) (Ok (1 1)))
    ((1 1 0 0) (Ok (1 1 0)))
    ((1 1 0 1) (Ok (1 1 0)))
    ((2) (Ok ()))
    ((2 0) (Ok (2)))
    ((2 1) (Ok (2))) |}]
;;

let%expect_test "[get]" =
  let test sexp =
    List.iter (Sexp_index.enumerate sexp) ~f:(fun i ->
      match Sexp_index.get i sexp with
      | Ok part -> print_s [%sexp (i : Sexp_index.t), (part : Sexp.t)]
      | Error error ->
        print_cr
          [%here]
          [%message "[get] failed" ~_:(i : Sexp_index.t) ~_:(error : Error.t)])
  in
  test the_sexp;
  [%expect
    {|
    (() ((foo 1) (bar ((2 3))) (baz 4)))
    ((0) (foo 1))
    ((0 0) foo)
    ((0 1) 1)
    ((1) (bar ((2 3))))
    ((1 0) bar)
    ((1 1) ((2 3)))
    ((1 1 0) (2 3))
    ((1 1 0 0) 2)
    ((1 1 0 1) 3)
    ((2) (baz 4))
    ((2 0) baz)
    ((2 1) 4) |}]
;;

let%expect_test "[set]" =
  let test sexp =
    List.iter (Sexp_index.enumerate sexp) ~f:(fun i ->
      match Sexp_index.set i sexp ~to_:(Atom "___") with
      | Ok part -> print_s [%sexp (i : Sexp_index.t), (part : Sexp.t)]
      | Error error ->
        print_cr
          [%here]
          [%message "[set] failed" ~_:(i : Sexp_index.t) ~_:(error : Error.t)])
  in
  test the_sexp;
  [%expect
    {|
    (() ___)
    ((0) (___ (bar ((2 3))) (baz 4)))
    ((0 0) ((___ 1) (bar ((2 3))) (baz 4)))
    ((0 1) ((foo ___) (bar ((2 3))) (baz 4)))
    ((1) ((foo 1) ___ (baz 4)))
    ((1 0) ((foo 1) (___ ((2 3))) (baz 4)))
    ((1 1)
     ((foo 1)
      (bar ___)
      (baz 4)))
    ((1 1 0) ((foo 1) (bar (___)) (baz 4)))
    ((1 1 0 0) ((foo 1) (bar ((___ 3))) (baz 4)))
    ((1 1 0 1) ((foo 1) (bar ((2 ___))) (baz 4)))
    ((2) ((foo 1) (bar ((2 3))) ___))
    ((2 0) ((foo 1) (bar ((2 3))) (___ 4)))
    ((2 1) ((foo 1) (bar ((2 3))) (baz ___))) |}]
;;

let%expect_test "[remove]" =
  let test sexp =
    List.iter (Sexp_index.enumerate sexp) ~f:(fun i ->
      print_s [%sexp (i : Sexp_index.t), (Sexp_index.remove i sexp : Sexp.t Or_error.t)])
  in
  test the_sexp;
  [%expect
    {|
    (() (Error "Cannot remove entire sexp."))
    ((0) (Ok ((bar ((2 3))) (baz 4))))
    ((0 0) (Ok ((1) (bar ((2 3))) (baz 4))))
    ((0 1) (Ok ((foo) (bar ((2 3))) (baz 4))))
    ((1)
     (Ok (
       (foo 1)
       (baz 4))))
    ((1 0) (Ok ((foo 1) (((2 3))) (baz 4))))
    ((1 1) (Ok ((foo 1) (bar) (baz 4))))
    ((1 1 0) (Ok ((foo 1) (bar ()) (baz 4))))
    ((1 1 0 0) (Ok ((foo 1) (bar ((3))) (baz 4))))
    ((1 1 0 1) (Ok ((foo 1) (bar ((2))) (baz 4))))
    ((2) (Ok ((foo 1) (bar ((2 3))))))
    ((2 0) (Ok ((foo 1) (bar ((2 3))) (4))))
    ((2 1) (Ok ((foo 1) (bar ((2 3))) (baz)))) |}]
;;
