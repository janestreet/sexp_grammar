open! Base
open! Import

(* Test all sexp grammars which are defined with [coerce] *)

(* Functor to combine quickcheck from [Core] with sexp grammar from [Base] *)
module Test
    (Quickcheckable : Core.Quickcheckable.S)
    (Sexp_grammar : sig
       type t [@@deriving sexp, sexp_grammar]
     end) =
struct
  include Quickcheckable
  include Sexp_grammar
end

module Value1 = Core.Int
module Value2 = Core.String

let%expect_test "[Hash_set.m__t_sexp_grammar]" =
  (module Test
            (struct
              type t = Core.Hash_set.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = Base.Hash_set.M(Value1).t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Hashtbl.m__t_sexp_grammar]" =
  (module Test
            (struct
              type t = Value2.t Core.Hashtbl.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = Value2.t Base.Hashtbl.M(Value1).t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar String)))
            Empty))))))))
    |}]
;;

let%expect_test "[Map.m__t_sexp_grammar]" =
  (module Test
            (struct
              type t = Value2.t Core.Map.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = Value2.t Base.Map.M(Value1).t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar String)))
            Empty))))))))
    |}]
;;

let%expect_test "[Set.m__t_sexp_grammar]" =
  (module Test
            (struct
              type t = Core.Set.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = Base.Set.M(Value1).t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Sexpable.Of_stringable(_)]" =
  let module T = struct
    type t = Core.Int.t [@@deriving quickcheck]

    include Base.Sexpable.Of_stringable (Core.Int)
  end
  in
  (module Test (T) (T)) |> Sexp_grammar_validation.validate_grammar |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Option_array]" =
  (module Test
            (struct
              type t = Value1.t Core.Option_array.t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Option_array.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many (Option Integer))) |}]
;;

let%expect_test "[Uniform_array]" =
  (module Test
            (struct
              type t = Value1.t Core.Uniform_array.t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Uniform_array.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Hash_set.Poly]" =
  (module Test
            (struct
              type t = Core.Hash_set.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Hash_set.Poly.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Hashtbl.Poly]" =
  (module Test
            (struct
              type t = Value2.t Core.Hashtbl.M(Value1).t [@@deriving quickcheck]
            end)
            (struct
              type t = (Value1.t, Value2.t) Base.Hashtbl.Poly.t
              [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar String)))
            Empty))))))))
    |}]
;;

let%expect_test "[Stack]" =
  (module Test
            (struct
              type t = Value1.t Core.Stack.t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Stack.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Queue]" =
  (module Test
            (struct
              type t = Value1.t Core.Queue.t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Queue.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Linked_queue]" =
  (module Test
            (struct
              type t = Value1.t Core.Linked_queue.t [@@deriving quickcheck]
            end)
            (struct
              type t = Value1.t Base.Linked_queue.t [@@deriving sexp, sexp_grammar]
            end))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Uchar]" =
  (module Test (Core.Uchar) (Base.Uchar))
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;
