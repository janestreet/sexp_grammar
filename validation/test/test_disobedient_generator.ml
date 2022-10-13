open! Base
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
  [%expect {| (wasted 19.67%) |}];
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
  [%expect {| (wasted 5.77%) |}];
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
  [%expect {| (wasted 18.23%) |}];
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
  [%expect {| (wasted 58.92%) |}];
  ignore ()
;;
