open! Core
open! Import
open Disobedient_generator.Private

(* silence unused constructor warnings *)
[@@@warning "-37"]

(* silence unused type warnings *)
[@@@warning "-34"]

module type S = sig
  type t [@@deriving sexp_grammar]
end

(* This expect test is deterministic, but unstable. Small changes in percentages are
   expected if quickcheck distributions or random seeds change. *)
let%expect_test "Yield of invalid sexps" =
  let test (module M : S) =
    let generator = create_unfiltered M.t_sexp_grammar in
    let accepts =
      Staged.unstage (Sexp_grammar.validate_sexp M.t_sexp_grammar) >> Result.is_ok
    in
    Base_quickcheck.Test.with_sample_exn generator ~f:(fun sequence ->
      let valid = Sequence.count sequence ~f:accepts in
      let num_values = Sequence.length sequence in
      let wasted = Core.Percent.of_mult Float.(of_int valid /. of_int num_values) in
      print_s [%message (wasted : Core.Percent.t)])
  in
  (* variants *)
  test
    (module struct
      type t =
        | T0
        | T1 of [ `A of int list option ]
        | T2 of
            { required : bool * float
            ; optional : string option [@sexp.option]
            }
      [@@deriving sexp_grammar]
    end);
  [%expect {| (wasted 15.88%) |}];
  (* polymorphic variants *)
  test
    (module struct
      type t =
        [ `T0
        | `T1 of [ `A of int list option ]
        | `T2 of bool * int
        ]
      [@@deriving sexp_grammar]
    end);
  [%expect {| (wasted 5.39%) |}];
  (* records *)
  test
    (module struct
      type t =
        { bool : bool
        ; float : float
        ; int_list : int list
        }
      [@@deriving sexp_grammar]
    end);
  [%expect {| (wasted 11.35%) |}];
  (* very permissive record, as in some config files *)
  test
    (module struct
      type t =
        { default : bool [@default true]
        ; option : int option [@sexp.option]
        ; list : bool list [@sexp.list]
        }
      [@@deriving sexp_grammar] [@@allow_extra_fields]
    end);
  [%expect {| (wasted 57.81%) |}];
  ignore ()
;;

let%expect_test "Generating for [Any] doesn't infinite loop" =
  (* Creating the disobedient generator below used to succeed, but when you tried to
     generate a value from it, it would infinite loop. *)
  Expect_test_helpers_base.require_does_raise (fun () ->
    Disobedient_generator.create { untyped = Any "" });
  [%expect
    {|
    ("Cannot generate disobedient sexps for a grammar that accepts all sexps"
     (grammar (Any "")))
    |}]
;;

let%expect_test "Generating for [t = t list] doesn't infinite loop" =
  let generator =
    Disobedient_generator.create
      { untyped =
          Tycon
            ( "t"
            , []
            , [ { tycon = "t"; tyvars = []; grammar = List (Many (Recursive ("t", []))) }
              ] )
      }
  in
  Quickcheck.test_can_generate generator ~f:(fun _ -> true);
  [%expect {| |}]
;;

let%expect_test "max tries works for grammars that accept all sexps" =
  let generator =
    Disobedient_generator.Private.create_with_max_tries
      ~max_tries:1000
      { untyped = Union [ String; List (Many (Any "")) ] }
  in
  (match Quickcheck.random_value generator with
   | Error `Max_tries_exceeded -> ()
   | Ok (sexp, error) ->
     print_cr
       [%message
         "Unexpectedly generated disobedient sexp" (sexp : Sexp.t) (error : Error.t)]);
  [%expect {| |}]
;;
