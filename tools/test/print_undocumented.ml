open! Core

let print_undocumented = Sexp_grammar_tools.print_undocumented

(* Helper function to create a test sexp grammar *)
let test_grammar grammar = print_undocumented grammar ~debug:false
let test_grammar_debug grammar = print_undocumented grammar ~debug:true

let%expect_test "fully documented record" =
  let module M = struct
    type t =
      { foo : int (** This is foo *)
      ; bar : string (** This is bar *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect {| |}]
;;

let%expect_test "doc comments require ~tags_of_doc_comments flag" =
  let module M = struct
    type t =
      { foo : int (** This is foo *)
      ; bar : string (** This is bar *)
      }
    [@@deriving sexp_grammar]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    2 of this record's fields are undocumented:
    - foo
    - bar
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "partially documented record" =
  let module M = struct
    type t =
      { foo : int (** This is foo *)
      ; bar : string
      ; baz : bool
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    2 of this record's fields are undocumented:
    - bar
    - baz
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "completely undocumented record" =
  let module M = struct
    type t =
      { foo : int
      ; bar : string
      ; baz : bool
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    3 of this record's fields are undocumented:
    - foo
    - bar
    - baz
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "fully documented variant" =
  let module M = struct
    type t =
      | Foo (** This is Foo *)
      | Bar of int (** This is Bar *)
      | Baz of string * bool (** This is Baz *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect {| |}]
;;

let%expect_test "partially documented variant" =
  let module M = struct
    type t =
      | Foo (** This is Foo *)
      | Bar of int
      | Baz of string * bool (** This is Baz *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this variant's constructors are undocumented:
    - Bar
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "completely undocumented variant" =
  let module M = struct
    type t =
      | Foo
      | Bar of int
      | Baz of string * bool
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    3 of this variant's constructors are undocumented:
    - Foo
    - Bar
    - Baz
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "nested record in variant" =
  let module M = struct
    type inner =
      { x : int
      ; y : string (** documented y *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    type t =
      | Simple (** Simple constructor *)
      | Complex of inner (** Complex constructor *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this record's fields are undocumented:
    - x
    Here's one path by which this type is included in the sexp grammar:
    - Complex
    |}]
;;

let%expect_test "nested variant in record" =
  let module M = struct
    type inner =
      | A (** documented A *)
      | B
      | C (** documented C *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]

    type t = { field : inner (** The field *) }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this variant's constructors are undocumented:
    - B
    Here's one path by which this type is included in the sexp grammar:
    - field
    |}]
;;

let%expect_test "multiple undocumented types sorted by path" =
  let module M = struct
    type mostly_documented =
      { a : int (** a *)
      ; b : int (** b *)
      ; c : int
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    type half_documented =
      { x : int (** x *)
      ; y : int
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    type completely_undocumented =
      { foo : int
      ; bar : int
      ; baz : int
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    type t =
      { first : mostly_documented (** first *)
      ; second : half_documented (** second *)
      ; third : completely_undocumented (** third *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this record's fields are undocumented:
    - c
    Here's one path by which this type is included in the sexp grammar:
    - first

    1 of this record's fields are undocumented:
    - y
    Here's one path by which this type is included in the sexp grammar:
    - second

    3 of this record's fields are undocumented:
    - foo
    - bar
    - baz
    Here's one path by which this type is included in the sexp grammar:
    - third
    |}]
;;

let%expect_test "debug mode shows all fields with their docs" =
  let module M = struct
    type t =
      { foo : int (** This is foo *)
      ; bar : string
      ; baz : bool (** This is baz *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar_debug [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this record's fields are undocumented:
    | foo | This is foo                          |
    | bar |                                      |
    | baz | This is baz                          |
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "debug mode for variants" =
  let module M = struct
    type t =
      | Foo (** Foo doc *)
      | Bar
      | Baz (** Baz doc *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar_debug [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this variant's constructors are undocumented:
    | Foo | Foo doc                              |
    | Bar |                                      |
    | Baz | Baz doc                              |
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "deeply nested structure" =
  let module M = struct
    type level3 = { deep : int } [@@deriving sexp_grammar ~tags_of_doc_comments]

    type level2 = Nested of level3 (** nested *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]

    type level1 = { middle : level2 (** middle *) }
    [@@deriving sexp_grammar ~tags_of_doc_comments]

    type t = { top : level1 (** top *) } [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this record's fields are undocumented:
    - deep
    Here's one path by which this type is included in the sexp grammar:
    - top
    - middle
    - Nested
    |}]
;;

let%expect_test "empty record" =
  let module M = struct
    type t = unit [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect {| |}]
;;

let%expect_test "single field record" =
  let module M = struct
    type t = { only_field : int } [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this record's fields are undocumented:
    - only_field
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "single constructor variant" =
  let module M = struct
    type t = OnlyOne [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  [%expect
    {|
    1 of this variant's constructors are undocumented:
    - OnlyOne
    Here's one path by which this type is included in the sexp grammar:
    (toplevel)
    |}]
;;

let%expect_test "primitive types produce no output" =
  test_grammar [%sexp_grammar: int];
  [%expect {| |}];
  test_grammar [%sexp_grammar: string];
  [%expect {| |}];
  test_grammar [%sexp_grammar: bool];
  [%expect {| |}];
  test_grammar [%sexp_grammar: float];
  [%expect {| |}]
;;

let%expect_test "shortest path is shown when type appears multiple places" =
  let module M = struct
    type inner = { x : int } [@@deriving sexp_grammar ~tags_of_doc_comments]

    type longer = Longer of inner (** longer path *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]

    type longest = Longest of longer (** longest path *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-37"]

    type t =
      { short : inner (** short path *)
      ; longer : longer (** longer path *)
      ; longest : longest (** longest path *)
      }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  (* Should show the shortest path through "short" *)
  [%expect
    {|
    1 of this record's fields are undocumented:
    - x
    Here's one path by which this type is included in the sexp grammar:
    - short
    |}]
;;

let%expect_test "tycon args are traversed (regression)" =
  let module M = struct
    type a = { no_doc : int } [@@deriving sexp_grammar ~tags_of_doc_comments]

    type 'a b =
      | Empty (** Empty *)
      | Cons of
          { field : 'a (** a field *)
          ; more : 'a b (** recurse *)
          } (** Cons *)
    [@@deriving sexp_grammar ~tags_of_doc_comments] [@@warning "-unused-constructor"]

    type t = { field_of_t : a b (** should be in path *) }
    [@@deriving sexp_grammar ~tags_of_doc_comments]
  end
  in
  test_grammar [%sexp_grammar: M.t];
  (* Should find [M.a] is missing a doc. *)
  [%expect
    {|
    1 of this record's fields are undocumented:
    - no_doc
    Here's one path by which this type is included in the sexp grammar:
    - field_of_t
    |}]
;;
