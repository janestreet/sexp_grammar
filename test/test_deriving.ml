(* This file tests all of the grammars in [Test_coverage_for_deriving]. We make sure to
   copy the derived sexp definitions from there. We additionally derive quickcheck
   generators here so that [ppx_sexp_conv] tests don't need to use [ppx_quickcheck].

   Recall that [Test_coverage_for_deriving] sometimes uses annotations to generate
   grammars other than the default for that type. If a type has a surprising grammar,
   check its definition.
*)

open! Core
open Ppx_sexp_conv_test_sexp_grammar
open Sexp_grammar_validation

type abstract_a = Test_coverage_for_deriving.abstract_a [@@deriving sexp, sexp_grammar]

let%expect_test _ =
  show_grammar
    (module struct
      type t = abstract_a [@@deriving sexp_grammar]
    end);
  [%expect {| (Any Test_coverage_for_deriving.abstract_a) |}]
;;

type abstract_b = Test_coverage_for_deriving.abstract_b [@@deriving sexp, sexp_grammar]

let%expect_test _ =
  show_grammar
    (module struct
      type t = abstract_b [@@deriving sexp_grammar]
    end);
  [%expect {| (Any Test_coverage_for_deriving.abstract_b) |}]
;;

include struct
  type integer = Test_coverage_for_deriving.integer [@@deriving sexp, sexp_grammar]
end

type integer = int [@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = integer [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| Integer |}]
;;

include struct
  type tuple = Test_coverage_for_deriving.tuple [@@deriving sexp, sexp_grammar]
end

type tuple = int * string [@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = tuple [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| (List (Cons Integer (Cons String Empty))) |}]
;;

include struct
  type pos = Test_coverage_for_deriving.pos [@@deriving sexp, sexp_grammar]
end

type pos = Test_coverage_for_deriving.pos =
  { x : float
  ; y : float
  }
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = pos [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (List
     (Fields
      ((allow_extra_fields false)
       (fields
        (((name x) (required true) (args (Cons Float Empty)))
         ((name y) (required true) (args (Cons Float Empty)))))))) |}]
;;

include struct
  type 'a unary = 'a Test_coverage_for_deriving.unary [@@deriving sexp, sexp_grammar]
end

type 'a unary = 'a list [@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = int unary [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| (List (Many Integer)) |}]
;;

include struct
  type enum = Test_coverage_for_deriving.enum [@@deriving sexp, sexp_grammar]
end

type enum = Test_coverage_for_deriving.enum =
  | One
  | Two
  | Three
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = enum [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Capitalized)
      (clauses
       (((name One) (clause_kind Atom_clause))
        ((name Two) (clause_kind Atom_clause))
        ((name Three) (clause_kind Atom_clause)))))) |}]
;;

include struct
  type ('a, 'b) which = ('a, 'b) Test_coverage_for_deriving.which
  [@@deriving sexp, sexp_grammar]
end

type ('a, 'b) which = ('a, 'b) Test_coverage_for_deriving.which =
  | This of 'a
  | That of 'b
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = (int, string) which [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Capitalized)
      (clauses
       (((name This) (clause_kind (List_clause (args (Cons Integer Empty)))))
        ((name That) (clause_kind (List_clause (args (Cons String Empty))))))))) |}]
;;

include struct
  type 'a optional = 'a Test_coverage_for_deriving.optional
  [@@deriving sexp, sexp_grammar]
end

type 'a optional = 'a Test_coverage_for_deriving.optional =
  | No
  | Yes of 'a
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = int optional [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Capitalized)
      (clauses
       (((name No) (clause_kind Atom_clause))
        ((name Yes) (clause_kind (List_clause (args (Cons Integer Empty))))))))) |}]
;;

include struct
  type empty = Test_coverage_for_deriving.empty [@@deriving sexp, sexp_grammar]
end

type empty = Test_coverage_for_deriving.empty = |

let%expect_test _ =
  show_grammar
    (module struct
      type t = empty [@@deriving sexp_grammar]
    end);
  [%expect {| (Union ()) |}]
;;

include struct
  type 'a phantom = 'a Test_coverage_for_deriving.phantom [@@deriving sexp, sexp_grammar]
end

type 'a phantom = int [@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = string phantom [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect {| Integer |}]
;;

include struct
  type color = Test_coverage_for_deriving.color [@@deriving sexp_poly, sexp_grammar]
end

type color =
  [ `Red
  | `Blue
  ]
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = color [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Any_case)
      (clauses
       (((name Red) (clause_kind Atom_clause))
        ((name Blue) (clause_kind Atom_clause)))))) |}]
;;

include struct
  type adjective = Test_coverage_for_deriving.adjective
  [@@deriving sexp_poly, sexp_grammar]
end

type adjective =
  [ color
  | `Fast
  | `Slow
  | `Count of int
  ]
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = adjective [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Union
     ((Variant
       ((name_kind Any_case)
        (clauses
         (((name Red) (clause_kind Atom_clause))
          ((name Blue) (clause_kind Atom_clause))))))
      (Variant
       ((name_kind Any_case)
        (clauses
         (((name Fast) (clause_kind Atom_clause))
          ((name Slow) (clause_kind Atom_clause))
          ((name Count) (clause_kind (List_clause (args (Cons Integer Empty))))))))))) |}]
;;

include struct
  type 'a tree = 'a Test_coverage_for_deriving.tree [@@deriving sexp, sexp_grammar]
end

type 'a tree = 'a Test_coverage_for_deriving.tree =
  { data : 'a
  ; children : 'a tree list
  }
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = int tree [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Recursive
     (Tycon tree (Integer))
     ((
      (tycon tree)
      (tyvars (a))
      (grammar
       (List
        (Fields
         ((allow_extra_fields false)
          (fields
           (((name data) (required true) (args (Cons (Tyvar a) Empty)))
            ((name children)
             (required true)
             (args (Cons (List (Many (Tycon tree ((Tyvar a))))) Empty)))))))))))) |}]
;;

include struct
  type alpha = Test_coverage_for_deriving.alpha [@@deriving sexp, sexp_grammar]
end

type alpha = int [@@deriving quickcheck]

let%expect_test _ =
  (* The types [alpha], [beta], and [gamma] are originally defined mutually recursively.
     The expansion of [[@@deriving sexp_grammar]] "shakes" some non-recursive grammars out
     of mutually recursive blocks, but not others. Here we see that [gamma] is no longer
     included, whereas [alpha] is included despite not being recursive. *)
  validate_grammar
    (module struct
      type t = alpha [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Recursive
     (Tycon alpha ())
     (((tycon alpha) (tyvars ()) (grammar Integer))
      ((tycon beta)
       (tyvars ())
       (grammar
        (List
         (Fields
          ((allow_extra_fields false)
           (fields
            (((name alpha) (required true) (args (Cons (Tycon alpha ()) Empty)))
             ((name betas)
              (required true)
              (args (Cons (List (Many (Tycon beta ()))) Empty)))))))))))) |}]
;;

include struct
  type beta = Test_coverage_for_deriving.beta [@@deriving sexp, sexp_grammar]
end

type beta = Test_coverage_for_deriving.beta =
  { alpha : alpha
  ; betas : beta list
  }
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = beta [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Recursive
     (Tycon beta ())
     (((tycon alpha) (tyvars ()) (grammar Integer))
      ((tycon beta)
       (tyvars ())
       (grammar
        (List
         (Fields
          ((allow_extra_fields false)
           (fields
            (((name alpha) (required true) (args (Cons (Tycon alpha ()) Empty)))
             ((name betas)
              (required true)
              (args (Cons (List (Many (Tycon beta ()))) Empty)))))))))))) |}]
;;

include struct
  type gamma = Test_coverage_for_deriving.gamma [@@deriving sexp, sexp_grammar]
end

type gamma = beta list [@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = gamma [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (List
     (Many
      (Recursive
       (Tycon beta ())
       (((tycon alpha) (tyvars ()) (grammar Integer))
        ((tycon beta)
         (tyvars ())
         (grammar
          (List
           (Fields
            ((allow_extra_fields false)
             (fields
              (((name alpha) (required true) (args (Cons (Tycon alpha ()) Empty)))
               ((name betas)
                (required true)
                (args (Cons (List (Many (Tycon beta ()))) Empty)))))))))))))) |}]
;;

include struct
  type record_attributes = Test_coverage_for_deriving.record_attributes
  [@@deriving sexp, sexp_grammar]
end

type record_attributes = Test_coverage_for_deriving.record_attributes =
  { a : int
  ; b : bool
  ; c : float option
  ; d : string list
  ; e : bytes array
  ; f : Sexp.t
  }
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    ~test_count:1_000
    (* hard to generate failing sexps, too many attempts to modify "extra" fields *)
    (module struct
      type t = record_attributes [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (List
     (Fields
      ((allow_extra_fields true)
       (fields
        (((name a) (required false) (args (Cons Integer Empty)))
         ((name b) (required false) (args Empty))
         ((name c) (required false) (args (Cons Float Empty)))
         ((name d) (required false) (args (Cons (List (Many String)) Empty)))
         ((name e) (required false) (args (Cons (List (Many String)) Empty)))
         ((name f) (required false) (args (Cons (Any Sexp.t) Empty)))))))) |}]
;;

include struct
  type variant_attributes = Test_coverage_for_deriving.variant_attributes
  [@@deriving sexp, sexp_grammar]
end

type variant_attributes = Test_coverage_for_deriving.variant_attributes =
  | A
  | B of int list
  | C of
      { a : int
      ; b : bool
      ; c : float option
      ; d : string list
      ; e : bytes array
      ; f : Sexp.t
      }
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    ~test_count:1_000
    (* hard to generate failing sexps, too many attempts to modify "extra" fields *)
    (module struct
      type t = variant_attributes [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Capitalized)
      (clauses
       (((name A) (clause_kind Atom_clause))
        ((name B) (clause_kind (List_clause (args (Many Integer)))))
        ((name C)
         (clause_kind
          (List_clause
           (args
            (Fields
             ((allow_extra_fields true)
              (fields
               (((name a) (required false) (args (Cons Integer Empty)))
                ((name b) (required false) (args Empty))
                ((name c) (required false) (args (Cons Float Empty)))
                ((name d)
                 (required false)
                 (args (Cons (List (Many String)) Empty)))
                ((name e)
                 (required false)
                 (args (Cons (List (Many String)) Empty)))
                ((name f) (required false) (args (Cons (Any Sexp.t) Empty))))))))))))))) |}]
;;

include struct
  type polymorphic_variant_attributes =
    Test_coverage_for_deriving.polymorphic_variant_attributes
  [@@deriving sexp_poly, sexp_grammar]
end

type polymorphic_variant_attributes =
  [ `A
  | `B of int list
  ]
[@@deriving quickcheck]

let%expect_test _ =
  validate_grammar
    (module struct
      type t = polymorphic_variant_attributes [@@deriving quickcheck, sexp, sexp_grammar]
    end)
  |> ok_exn;
  [%expect
    {|
    (Variant
     ((name_kind Any_case)
      (clauses
       (((name A) (clause_kind Atom_clause))
        ((name B) (clause_kind (List_clause (args (Many Integer))))))))) |}]
;;

include struct
  type opaque = Test_coverage_for_deriving.opaque [@@deriving sexp, sexp_grammar]
end

type opaque = Test_coverage_for_deriving.opaque =
  { x : string
  ; y : int -> int
  }

let%expect_test _ =
  show_grammar
    (module struct
      type t = opaque [@@deriving sexp_grammar]
    end);
  [%expect
    {|
    (List
     (Fields
      ((allow_extra_fields false)
       (fields
        (((name x) (required true) (args (Cons (Union ()) Empty)))
         ((name y) (required true) (args (Cons (Union ()) Empty)))))))) |}]
;;
