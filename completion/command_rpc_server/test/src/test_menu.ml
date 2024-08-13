open! Core
open! Async
open! Import

let%expect_test "show menu" =
  let%bind () = run "../bin/main.exe" [ "test-menu"; "-menu" ] in
  [%expect {| ((sexp-grammar-complete 1)(sexp-grammar-complete 2)) |}];
  return ()
;;
