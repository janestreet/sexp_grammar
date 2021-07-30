open! Base
open! Import

(* disable warning for unused constructors *)
[@@@warning "-37"]

(** Test that our quickcheck generators produce a good sample of the possible values. *)

module type S = sig
  type t [@@deriving sexp_grammar]
end

let test ?config (module M : S) ~f =
  require_does_not_raise [%here] (fun () ->
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Sexp.t [@@deriving sexp_of]

        let quickcheck_generator = Obedient_generator.create M.t_sexp_grammar
        let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
      end)
      ?config
      ~f)
;;

let%expect_test "generator of an enumerable type will exhaust the universe" =
  let module Exhaustiveness = struct
    module type S = sig
      type t [@@deriving enumerate, sexp, sexp_grammar]

      include Comparator.S with type t := t
    end
  end
  in
  let test_exhaustiveness (module M : Exhaustiveness.S) =
    let seen = ref (Set.empty (module M)) in
    test (module M) ~f:(fun sexp -> seen := Set.add !seen (M.t_of_sexp sexp));
    require_sets_are_equal [%here] (module M) !seen (Set.of_list (module M) M.all)
  in
  (* variant *)
  test_exhaustiveness
    (module struct
      module T = struct
        type t =
          | T0
          | T1 of bool
          | T2 of bool option * bool
          | Tf of
              { required : bool
              ; optional : bool option [@sexp.option]
              }
          | T_poly of [ `A | `B of bool ]
        [@@deriving compare, enumerate, hash, sexp, sexp_grammar]
      end

      include T
      include Comparator.Make (T)
    end);
  [%expect {| |}];
  (* polymorphic variant *)
  test_exhaustiveness
    (module struct
      module T = struct
        type u = [ `U ] [@@deriving compare, enumerate, hash, sexp, sexp_grammar]

        type t =
          [ `T of bool option
          | u
          ]
        [@@deriving compare, enumerate, hash, sexp, sexp_grammar]
      end

      include T
      include Comparator.Make (T)
    end);
  [%expect {| |}];
  ignore ()
;;

(* Records have multiple valid sexp representations, so we have to look at the sexps
   rather than the parsed values. *)
let%expect_test "sexps of records" =
  let config =
    { Base_quickcheck.Test.default_config with
      test_count = 100
    ; sizes = Sequence.repeat 3
    }
  in
  let test_record (module M : S) =
    let seen = ref (Set.empty (module Sexp)) in
    test (module M) ~config ~f:(fun sexp -> seen := Set.add !seen sexp);
    Ref.set_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
      Set.iter !seen ~f:print_s)
  in
  let module X = struct
    type t = [ `X ] [@@deriving sexp_grammar]
  end
  in
  (* sexp.option *)
  test_record
    (module struct
      type t = { sexp_option : X.t option [@sexp.option] } [@@deriving sexp_grammar]
    end);
  [%expect {|
    ()
    ((sexp_option X)) |}];
  (* sexp.list *)
  test_record
    (module struct
      type t = { sexp_list : X.t list [@sexp.list] } [@@deriving sexp_grammar]
    end);
  [%expect
    {|
    ()
    ((sexp_list ()))
    ((sexp_list (X)))
    ((sexp_list (X X)))
    ((sexp_list (X X X))) |}];
  (* default *)
  test_record
    (module struct
      type t = { default : X.t [@sexp.default X] } [@@deriving sexp_grammar]
    end);
  [%expect {|
    ()
    ((default X)) |}];
  (* unordered *)
  test_record
    (module struct
      type t =
        { first : X.t
        ; second : X.t
        }
      [@@deriving sexp_grammar]
    end);
  [%expect {|
    ((first X) (second X))
    ((second X) (first X)) |}];
  (* extra fields *)
  test_record
    (module struct
      type t = { required : X.t } [@@deriving sexp_grammar] [@@allow_extra_fields]
    end);
  (* This output is pseudorandom and unstable. It's okay to change it without scrutiny. *)
  [%expect
    {|
    ((B 1 y ()) (required X))
    ((BdM) (required X))
    ((C) (A) (required X))
    ((D) (vK 4 "") (required X))
    ((DRh () "" T) (required X))
    ((E Qa) (required X))
    ((ERV) (required X))
    ((I () (())) (required X) (p) (E ((()))))
    ((L) (T (()) ()) (required X) (f () () ""))
    ((M () () ()) (required X))
    ((O () P I) (required X))
    ((P (q d)) (q (() ())) (required X) (A () e 6))
    ((S) (p () (v)) (r) (required X))
    ((SLC ()) (required X))
    ((U) (M) (R L () ()) (required X))
    ((W ()) (required X) (f) (Q))
    ((Wn "\000s") (required X))
    ((Y () ()) (required X))
    ((Z) (J ()) (required X))
    ((Z 8X) (required X) (O ()) (d))
    ((Z () ()) (required X))
    ((ZUi 7js) (required X))
    ((arM ()) (required X))
    ((b) (required X))
    ((b) (required X) (J ()) (y))
    ((eSZ F "" "") (required X))
    ((f ("") ()) (m) (required X))
    ((i) (E ("" "")) (required X) (Z () ()))
    ((i (())) (T bA) (required X) (R))
    ((jGi J3) (required X))
    ((l) (W () () i) (required X))
    ((n) (RH M (())) (required X))
    ((p 8eU) (j) (M) (required X))
    ((r) (L ()) (u () () "") (required X))
    ((r () ()) (z) (O) (required X))
    ((required X))
    ((required X) (A ()) (lv 2))
    ((required X) (AKy (() ())))
    ((required X) (B "" () "") (W ((()))) (y))
    ((required X) (B (())) (i b7) (O))
    ((required X) (Cn "" M ()) (z ()))
    ((required X) (E H ()))
    ((required X) (J (())) (v ()) (B ""))
    ((required X) (Kb "\255k") (A c a "\255"))
    ((required X) (M))
    ((required X) (ON "" () ()))
    ((required X) (OOv))
    ((required X) (PnG () ""))
    ((required X) (Q x))
    ((required X) (QWW "" ()))
    ((required X) (R () _ B))
    ((required X) (Y))
    ((required X) (aQ))
    ((required X) (aS () ()))
    ((required X) (eS ugl))
    ((required X) (fZ () "" ()) (S))
    ((required X) (g () () V) (x) (d))
    ((required X) (is () "") (Q () "\255" ()))
    ((required X) (kTz))
    ((required X) (l () (a)) (w "" () "") (n "" () ()))
    ((required X) (yb p ()))
    ((required X) (ztL))
    ((sG ((G))) (required X))
    ((t) (H () () "") (required X) (v (() i)))
    ((v ()) (required X))
    ((v (g "")) (required X))
    ((w (Q) ()) (required X) (B () z) (L))
    ((x ()) (required X)) |}];
  ignore ()
;;

(* This test will occasionally need to be updated. It is not necessary to scrutinize the
   new output. Quickcheck generators are deterministic, but not necessarily stable. *)
let%expect_test "generator of an infinite type produces a good distribution of sizes" =
  let module Rstats = Jane_kernel.Rstats in
  let rec size_of_sexp sexp =
    match (sexp : Sexp.t) with
    | Atom _ -> 1
    | List list -> 1 + List.sum (module Int) list ~f:size_of_sexp
  in
  let test_stats (module M : S) =
    let rstats = Rstats.create () in
    test
      (module M)
      ~f:(fun sexp -> Rstats.update_in_place rstats (Float.of_int (size_of_sexp sexp)));
    print_s
      [%sexp
        { samples = (Rstats.samples rstats : int)
        ; max = (Rstats.max rstats : float)
        ; mean = (Rstats.mean rstats : float)
        ; min = (Rstats.min rstats : float)
        ; stdev = (Rstats.stdev rstats : float)
        }]
  in
  let module M = struct
    type t =
      | T of u
      | T0 of [ `A | `B ]

    and u =
      | U of t
      | U0 of
          { required : int
          ; optional : bool [@default true]
          }
    [@@deriving sexp_grammar]
  end
  in
  test_stats (module M);
  [%expect
    {|
    ((samples 10000)
     (max     31)
     (mean    6.1212)
     (min     3)
     (stdev   3.8104543713645929)) |}]
;;
