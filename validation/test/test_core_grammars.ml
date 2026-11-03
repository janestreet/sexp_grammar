open! Core
open! Import

let%expect_test "[Doubly_linked]" =
  (module struct
    type t = int Doubly_linked.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Deque]" =
  (module struct
    type t = int Deque.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Md5]" =
  (module struct
    type t = Md5.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  (module struct
    type t = Md5.As_binary_string.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Percent.Stable.{V3,V2,Option.{V3,V2}}]" =
  let module Percent_quickcheck : Quickcheckable.S with type t := Percent.t = Percent in
  (module struct
    type t = Percent.Stable.V3.t [@@deriving sexp, sexp_grammar]

    include Percent_quickcheck
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  (module struct
    type t = Percent.Stable.V2.t [@@deriving sexp, sexp_grammar]

    include Percent_quickcheck
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  let module Percent_option_quickcheck :
    Quickcheckable.S with type t := Percent.Option.t =
    Percent.Option
  in
  (module struct
    type t = Percent.Stable.Option.V3.t [@@deriving sexp, sexp_grammar]

    include Percent_option_quickcheck
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (Option String) |}];
  (module struct
    type t = Percent.Stable.Option.V2.t [@@deriving sexp, sexp_grammar]

    include Percent_option_quickcheck
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (Option String) |}]
;;

let%expect_test "[Time_float.Ofday]" =
  (module struct
    type t = Time_float.Ofday.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Time_float.Stable.With_utc_sexp.V2]" =
  (module struct
    type t = Time_float.Stable.With_utc_sexp.V2.t [@@deriving sexp, sexp_grammar]

    include (Time_float : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (Any Sexp.t) |}]
;;

let%expect_test "[Time_ns.Ofday]" =
  (module struct
    type t = Time_ns.Ofday.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect
    {|
    (Tagged
     ((key sexp_grammar.type_name) (value Core.Time_ns.Ofday.t) (grammar String)))
    |}]
;;

let%expect_test "[Time_ns.Option.Alternate_sexp]" =
  (module struct
    type t = Time_ns.Option.Alternate_sexp.t [@@deriving sexp, sexp_grammar]

    include (Time_ns.Option : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect
    {|
    (Option
     (Tagged
      ((key sexp_grammar.type_name)
       (value Core.Time_ns.Alternate_sexp.t)
       (grammar String))))
    |}]
;;

let%expect_test "[Sexp.With_text]" =
  (module struct
    type t = int Sexp.With_text.t [@@deriving sexp, sexp_grammar]

    include
      Quickcheckable.Of_quickcheckable
        (Int)
        (struct
          type nonrec t = t

          let of_quickcheckable int = Sexp.With_text.of_value Int.sexp_of_t int
          let to_quickcheckable = Sexp.With_text.value
        end)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Map.m__t_sexp_grammar]" =
  (module struct
    type t = int Map.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
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
            (Tagged ((key sexp_grammar.assoc.value) (value ()) (grammar Integer)))
            Empty))))))))
    |}]
;;

let%expect_test "[Set.m__t_sexp_grammar]" =
  (module struct
    type t = Set.M(Core.Int).t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test _ =
  let module Stringable_int = struct
    include Int
    include Sexpable.Of_stringable (Int)
  end
  in
  (module struct
    type t = Stringable_int.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Time_float.Stable.Span.{V1,V2,V3}]" =
  (module struct
    type t = Time_float.Stable.Span.V1.t [@@deriving sexp, sexp_grammar]

    include (Time_float.Span : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  (module struct
    type t = Time_float.Stable.Span.V2.t [@@deriving sexp, sexp_grammar]

    include (Time_float.Span : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  (module struct
    type t = Time_float.Stable.Span.V3.t [@@deriving sexp, sexp_grammar]

    include (Time_float.Span : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Time_ns.Stable.Span.{V1,V2}]" =
  (module struct
    type t = Time_ns.Stable.Span.V1.t [@@deriving sexp, sexp_grammar]

    include (Time_ns.Span : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}];
  (module struct
    type t = Time_ns.Stable.Span.V2.t [@@deriving sexp, sexp_grammar]

    include (Time_ns.Span : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Date.Stable.{V1,Option.V1}]" =
  (module struct
    type t = Date.Stable.V1.t [@@deriving sexp, sexp_grammar]

    include (Date : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect
    {| (Tagged ((key sexp_grammar.type_name) (value Core.Date.t) (grammar String))) |}];
  (module struct
    type t = Date.Stable.Option.V1.t [@@deriving sexp, sexp_grammar]

    include (Date.Option : Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect
    {|
    (Option
     (Tagged ((key sexp_grammar.type_name) (value Core.Date.t) (grammar String))))
    |}]
;;

let%expect_test "[Map.Poly{,.Tree}]" =
  (module struct
    type t = (int, string) Map.Poly.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
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
    |}];
  (module struct
    type t = (int, string) Map.Poly.Tree.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
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

let%expect_test "[Set.Poly{,.Tree}]" =
  let module Set_poly_quickcheckable = struct
    include Set.Poly

    include
      Quickcheckable.Of_quickcheckable1
        (List)
        (struct
          type nonrec 'a t = 'a t

          let to_quickcheckable = Set.to_list
          let of_quickcheckable = of_list
        end)
  end
  in
  (module struct
    type t = int Set_poly_quickcheckable.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}];
  let module Set_poly_tree_quickcheckable = struct
    include Set.Poly.Tree

    include
      Quickcheckable.Of_quickcheckable1
        (Set_poly_quickcheckable)
        (struct
          type nonrec 'a t = 'a t

          let to_quickcheckable = Set.Poly.of_tree
          let of_quickcheckable = Set.to_tree
        end)
  end
  in
  (module struct
    type t = int Set_poly_tree_quickcheckable.t
    [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Bigstring]" =
  (module struct
    type t = Bigstring.t [@@deriving sexp, sexp_grammar]

    include
      Quickcheckable.Of_quickcheckable
        (String)
        (struct
          type nonrec t = t

          let to_quickcheckable t = Bigstring.to_string t
          let of_quickcheckable string = Bigstring.of_string string
        end)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| String |}]
;;

let%expect_test "[Fdeque]" =
  (module struct
    type t = int Fdeque.t [@@deriving quickcheck, sexp, sexp_grammar]
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (List (Many Integer)) |}]
;;

let%expect_test "[Set_once.Stable.V1]" =
  (module struct
    type t = int Set_once.Stable.V1.t [@@deriving sexp, sexp_grammar]

    include (
    struct
      type t = int Set_once.t [@@deriving quickcheck]
    end :
      Quickcheckable.S with type t := t)
  end)
  |> Sexp_grammar_validation.validate_grammar
  |> require_ok;
  [%expect {| (Option Integer) |}]
;;

let%expect_test "[Pid]" =
  (module Pid) |> Sexp_grammar_validation.validate_grammar |> require_ok;
  [%expect {| Integer |}]
;;
