open! Core
open! Import

let%expect_test "smoke test" =
  let open struct
    [@@@warning "-37"]

    type t =
      | Foo of { foo : foo (** foo *) } (** Foo *)
      | Bar of bar (** Bar *)

    and foo =
      { ham : int (** ham *)
      ; spam : int (** spam *)
      }

    and bar =
      [ `snatch (** snatch *)
      | `clean of [ `jerk (** jerk *) ] (** clean *)
      ]
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end in
  let test = show (Sexp_grammar_completion.complete [%sexp_grammar: t] |> unstage) in
  test "";
  [%expect
    {|
    │
    =>
    (Bar │) (matches atom prefix)
    -- doc comment:
    > Bar
    (Foo │) (matches atom prefix)
    -- doc comment:
    > Foo
    |}];
  test "(Foo ";
  [%expect
    {|
    (Foo │
    =>
    (Foo (foo │) (matches atom prefix)
    -- doc comment:
    > foo
    |}];
  test "(Foo (foo ";
  [%expect
    {|
    (Foo (foo │
    =>
    (Foo (foo (│) (matches atom prefix)
    |}];
  test "(Foo (foo (";
  [%expect
    {|
    (Foo (foo (│
    =>
    (Foo (foo ((ham │)  (matches atom prefix)
    -- doc comment:
    > ham
    (Foo (foo ((spam │) (matches atom prefix)
    -- doc comment:
    > spam
    |}];
  test "(Bar ";
  [%expect
    {|
    (Bar │
    =>
    (Bar snatch│   (matches atom prefix)
    -- doc comment:
    > snatch
    (Bar (clean │) (matches atom prefix)
    -- doc comment:
    > clean
    |}];
  test "(Bar (clean ";
  [%expect
    {|
    (Bar (clean │
    =>
    (Bar (clean jerk│ (matches atom prefix)
    -- doc comment:
    > jerk
    |}];
  ()
;;

let%expect_test "whitespace in doc comments" =
  let open struct
    [@@@warning "-37"]

    (* Temporarily disable ocamlformat so that doc comments don't get formatted. *)
    [@@@ocamlformat "disable"]

    type t =
      | A
      (** Here is a

          multiline doc comment

          with some "special" characters too *)
      | B (**


             with leading and trailing lines


          *)
      | C (**   single line doc comment but still leading and trailing whitespace   *)
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    [@@@ocamlformat "enable"]
  end in
  let test = show (Sexp_grammar_completion.complete [%sexp_grammar: t] |> unstage) in
  test "";
  [%expect
    {|
    │
    =>
    A│ (matches atom prefix)
    -- doc comment:
    > Here is a
    >
    > multiline doc comment
    >
    > with some "special" characters too
    B│ (matches atom prefix)
    -- doc comment:
    > with leading and trailing lines
    C│ (matches atom prefix)
    -- doc comment:
    > single line doc comment but still leading and trailing whitespace
    |}]
;;

let%expect_test "keep showing documentation throughout completion" =
  (* variant *)
  let open struct
    [@@@warning "-37"]

    type t =
      | Aa (** Aa *)
      | Ab of int (** Ab *)
      | Ac of int (** Ac *)
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end in
  let test = show (Sexp_grammar_completion.complete [%sexp_grammar: t] |> unstage) in
  test "";
  [%expect
    {|
    │
    =>
    Aa│    (matches atom prefix)
    -- doc comment:
    > Aa
    (Ab │) (matches atom prefix)
    -- doc comment:
    > Ab
    (Ac │) (matches atom prefix)
    -- doc comment:
    > Ac
    |}];
  (* The completion server returns candidates that don't match the atom prefix. See
     [rpc_complete.mli] for an explanation of this design decision. *)
  test "A";
  [%expect
    {|
    A│
    =>
    Aa│    (matches atom prefix)
    -- doc comment:
    > Aa
    (Ab │)
    -- doc comment:
    > Ab
    (Ac │)
    -- doc comment:
    > Ac
    |}];
  test "(";
  [%expect
    {|
    (│
    =>
    (Ab│ (matches atom prefix)
    -- doc comment:
    > Ab
    (Ac│ (matches atom prefix)
    -- doc comment:
    > Ac
    |}];
  test "(A";
  [%expect
    {|
    (A│
    =>
    (Ab│ (matches atom prefix)
    -- doc comment:
    > Ab
    (Ac│ (matches atom prefix)
    -- doc comment:
    > Ac
    |}];
  (* record *)
  let open struct
    [@@@warning "-37"]

    type t =
      { aa : int (** aa *)
      ; ab : int (** ab *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end in
  let test = show (Sexp_grammar_completion.complete [%sexp_grammar: t] |> unstage) in
  test "";
  [%expect
    {|
    │
    =>
    (│) (matches atom prefix)
    |}];
  test "(";
  [%expect
    {|
    (│
    =>
    ((aa │) (matches atom prefix)
    -- doc comment:
    > aa
    ((ab │) (matches atom prefix)
    -- doc comment:
    > ab
    |}];
  test "((a";
  [%expect
    {|
    ((a│
    =>
    ((aa│ (matches atom prefix)
    -- doc comment:
    > aa
    ((ab│ (matches atom prefix)
    -- doc comment:
    > ab
    |}];
  test "(ab ";
  [%expect
    {|
    (ab │
    =>
    |}]
;;
