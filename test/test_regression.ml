open! Core
open! Expect_test_helpers_base

module Traverse = Sexp_grammar.Fold_recursive (struct
    type t = depth:int -> unit
    type list_t = (depth:int -> unit) list

    let atomic ~depth:_ = ()
    let compound ts ~depth = List.iter ts ~f:(fun t -> t ~depth)
    let any (_ : string) = atomic
    let bool = atomic
    let char = atomic
    let integer = atomic
    let float = atomic
    let string = atomic
    let option = Fn.id
    let union = compound
    let list = compound
    let empty = []
    let cons t ts = t :: ts
    let many t = [ t ]

    let record alist ~allow_extra_fields:_ =
      List.concat_map
        alist
        ~f:(fun ((_ : string), (field, (_ : (string * Sexp.t) list))) ->
          match (field : _ Sexp_grammar.Field.t) with
          | Optional x -> x
          | Required x -> x)
    ;;

    let variant cases ~case_sensitivity:_ =
      List.concat_map
        cases
        ~f:(fun ((_ : string), (case, (_ : (string * Sexp.t) list))) ->
          Option.value case ~default:[])
      |> compound
    ;;

    let lazy_ lazy_t = force lazy_t
    let tag t (_ : string) (_ : Sexp.t) = t

    let of_lazy_recursive lazy_t ~depth =
      if depth > 0 then (force lazy_t) ~depth:(depth - 1)
    ;;
  end)

let test ?cr ?(depth = 1) (module M : Sexp_grammar_validation.With_grammar) =
  require_does_not_raise ?cr (fun () ->
    Traverse.of_typed_grammar_exn M.t_sexp_grammar ~depth)
;;

(* Grammar validation should not fail when a type variable appears inside the body type
   expression of a recursion expression, e.g., ... (Recursive (Tycon r ((Tyvar a))) ...)
   ... *)
let%expect_test "tyvar inside recursion body" =
  test
    (module struct
      type 'a recursive = { self : 'a recursive } [@@deriving sexp_grammar]

      type 'b recursive_with_reference =
        { this : 'b recursive_with_reference
        ; that : 'b recursive
        }
      [@@deriving sexp_grammar]

      type t = int recursive_with_reference [@@deriving sexp_grammar]
    end);
  [%expect {| |}]
;;

(* Grammar validation should not fail when an earlier-defined type constructor appears
   inside the body type expression of a recursion expression, e.g., ... (Recursive (Tycon
   l ((Tycon t ()))) ...) ... *)
let%expect_test "tycon inside recursion body" =
  test
    (module struct
      type 'a u = U of 'a u [@@deriving quickcheck, sexp, sexp_grammar]
      type t = T of t u [@@deriving quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| |}]
;;

(* This test shows a case where a type can refer to another type of the same base name. *)
let%expect_test "tycon inside recursion body with same base name" =
  test
    (module struct
      module T = struct
        type 'a t = { this : 'a t } [@@deriving quickcheck, sexp, sexp_grammar]
      end

      type t = { that : t T.t } [@@deriving quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| |}]
;;

(* This test shows a case where a recursive type can transitively depend on another type
   of the same name where no explicit namespace qualification happens in the definition.
*)
let%expect_test "tycon inside recursion body with same explicitly qualified name" =
  test
    (module struct
      module T = struct
        type 'a t = { this : 'a t } [@@deriving quickcheck, sexp, sexp_grammar]
        type 'a u = 'a t [@@deriving quickcheck, sexp, sexp_grammar]
      end

      open T

      type t = { that : t u } [@@deriving quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| |}]
;;

(* This test shows a case where a type can transitively depend on another type which has
   the same name in (essentially) the same scope. *)
let%expect_test "tycon inside recursion body with same fully qualified name" =
  test
    (module struct
      open struct
        type 'a t = { this : 'a t } [@@deriving quickcheck, sexp, sexp_grammar]
        type 'a u = 'a t [@@deriving quickcheck, sexp, sexp_grammar]
      end

      type t = { that : t u } [@@deriving quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| |}]
;;

let%expect_test "[sexp_of] and [of_sexp] roundtrip" =
  let module M = struct
    type t = int Sexp_grammar.t [@@deriving equal, sexp_of]

    (* Equality modulo [Lazy] nodes, since serializers may drop them. *)
    let equal =
      Comparable.lift equal ~f:(fun x ->
        { untyped = Sexp_grammar.Eager_copy.of_typed_grammar x })
    ;;
  end
  in
  let module Sexp = struct
    type t = int Sexp_grammar.t [@@deriving sexp]
    type repr = Sexp.t [@@deriving sexp_of]

    let to_repr = sexp_of_t
    let of_repr = t_of_sexp
    let repr_name = "Sexp"
  end
  in
  let module Bin = struct
    module T = struct
      type t = int Sexp_grammar.t [@@deriving bin_io]
    end

    include T

    type repr = Bigstring.t [@@deriving sexp_of]

    let to_repr = Binable.to_bigstring (module T)
    let of_repr = Binable.of_bigstring (module T)
    let repr_name = "Binio"
  end
  in
  print_and_check_round_trip
    ~cr:CR_soon
    (module M)
    [ (module Sexp); (module Bin) ]
    [ { untyped = Integer }
    ; { untyped = Option Bool }
    ; { untyped = Union [] }
    ; { untyped = Union [ String; Any "any" ] }
    ; { untyped = Lazy (Portable_lazy.from_val Sexp_grammar.Float) }
    ; { untyped =
          Tycon
            ( "t"
            , [ Integer ]
            , [ { tycon = "t"
                ; tyvars = [ "a" ]
                ; grammar = List (Many (Recursive ("t", [ Tyvar "a" ])))
                }
              ] )
      }
    ];
  [%expect
    {|
    ((Sexp  Integer)
     (Binio "\003"))
    ((Sexp (Option Bool)) (Binio "\006\001"))
    ((Sexp (Union ())) (Binio "\t\000"))
    ((Sexp (Union (String (Any any)))) (Binio "\t\002\005\000\003any"))
    ((Sexp  Float)
     (Binio "\014\004"))
    ((Sexp (
       Tycon t
       (Integer)
       (((tycon t) (tyvars (a)) (grammar (List (Many (Recursive t ((Tyvar a))))))))))
     (Binio "\012\001t\001\003\001\001t\001\001a\007\002\r\001t\001\011\001a"))
    |}]
;;
