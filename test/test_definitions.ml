open! Core
open Expect_test_helpers_base
open Sexp_grammar_validation

(* Here we test a large grab-bag of types, including

   - builtin types
   - int63, which pretends to be a builtin type in its off hours
   - the types with [All _] grammars: sexp, info, error
   - functorized container types (map, set, hashtbl, hash_set)

   If we add new grammars in Base that are not trivial, we should test them somewhere.
   Here is okay, but somewhere under [lib/base/test] is okay too. *)

let%expect_test "unit" =
  validate_grammar (module Unit) |> ok_exn;
  [%expect {| (List Empty) |}]
;;

let%expect_test "bool" =
  validate_grammar (module Bool) |> ok_exn;
  [%expect {| Bool |}]
;;

let%expect_test "char" =
  validate_grammar (module Char) |> ok_exn;
  [%expect {| Char |}]
;;

let%expect_test "string" =
  validate_grammar (module String) |> ok_exn;
  [%expect {| String |}]
;;

let%expect_test "bytes" =
  validate_grammar (module Bytes) |> ok_exn;
  [%expect {| String |}]
;;

let%expect_test "float" =
  validate_grammar (module Float) |> ok_exn;
  [%expect {| Float |}]
;;

let%expect_test "int" =
  validate_grammar (module Int) |> ok_exn;
  [%expect {| Integer |}]
;;

let%expect_test "int32" =
  validate_grammar (module Int32) |> ok_exn;
  [%expect {| Integer |}]
;;

let%expect_test "int63" =
  validate_grammar (module Int63) |> ok_exn;
  [%expect {| Integer |}]
;;

let%expect_test "int64" =
  validate_grammar (module Int64) |> ok_exn;
  [%expect {| Integer |}]
;;

let%expect_test "nativeint" =
  validate_grammar (module Nativeint) |> ok_exn;
  [%expect {| Integer |}]
;;

let%expect_test "sexp" =
  validate_grammar (module Sexp) |> ok_exn;
  [%expect {| (Any Sexp.t) |}]
;;

let%expect_test "info" =
  show_grammar (module Info);
  [%expect {| (Any Info.t) |}]
;;

let%expect_test "error" =
  show_grammar (module Error);
  [%expect {| (Any Error.t) |}]
;;

let%expect_test "option" =
  let test ~read_old ~write_old =
    Dynamic.with_temporarily Sexplib.Conv.read_old_option_format read_old ~f:(fun () ->
      Dynamic.with_temporarily
        Sexplib.Conv.write_old_option_format
        write_old
        ~f:(fun () -> validate_grammar_poly1 (module Option)))
  in
  (* most combinations of read/write flags for options formats should work *)
  test ~read_old:true ~write_old:false |> require_ok;
  [%expect {| (Option (Any A)) |}];
  test ~read_old:true ~write_old:true |> require_ok;
  [%expect {| (Option (Any A)) |}];
  test ~read_old:false ~write_old:false |> require_ok;
  [%expect {| (Option (Any A)) |}];
  (* writing the old option format without reading it should fail *)
  test ~read_old:false ~write_old:true |> require_error [%sexp_of: unit] ~print_error:true;
  [%expect
    {|
    (Option (Any A))
    ("Base_quickcheck.Test.run: test failed"
      (input ())
      (error (
        "[t_sexp_grammar] rejects sexp from [sexp_of_t]"
        ("expected an option" (sexp ())))))
    |}]
;;

let%expect_test "list" =
  validate_grammar_poly1
    ~test_count:1_000 (* hard to generate failing sexps (only one way to go wrong) *)
    (module List)
  |> ok_exn;
  [%expect {| (List (Many (Any A))) |}]
;;

let%expect_test "array" =
  validate_grammar_poly1
    ~test_count:1_000 (* hard to generate failing sexps (only one way to go wrong) *)
    (module Array)
  |> ok_exn;
  [%expect {| (List (Many (Any A))) |}]
;;

let%expect_test "ref" =
  validate_grammar_poly1 (module Ref) |> ok_exn;
  [%expect {| (Any A) |}]
;;

let%expect_test "lazy_t" =
  validate_grammar_poly1 (module Lazy) |> ok_exn;
  [%expect {| (Any A) |}]
;;

let%expect_test "Set.t" =
  validate_grammar
    (module struct
      type t = Set.M(Int).t [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "Map.t" =
  validate_grammar_poly1
    (module struct
      type 'a t = 'a Map.M(Int).t [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar (Any A))))
            Empty))))))))
    |}]
;;

let%expect_test "Hash_set.t" =
  validate_grammar
    (module struct
      type t = Hash_set.M(Int).t [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "Hashtbl.t" =
  validate_grammar_poly1
    (module struct
      type 'a t = 'a Hashtbl.M(Int).t [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar (Any A))))
            Empty))))))))
    |}]
;;
