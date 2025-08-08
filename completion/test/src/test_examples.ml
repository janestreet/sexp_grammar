(** Tests on handwritten examples *)

open! Base
open! Import

let (foo_grammar : _ Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive
        ; clauses =
            [ No_tag { name = "foo"; clause_kind = Atom_clause }
            ; No_tag { name = "foo bar"; clause_kind = Atom_clause }
            ; No_tag { name = "\nnewline"; clause_kind = Atom_clause }
            ]
        }
  }
;;

let%expect_test "quoted atoms" =
  let complete = foo_grammar |> Sexp_grammar_completion.complete |> Staged.unstage in
  show complete "f";
  [%expect
    {|
    f│
    =>
    "\nnewline"│
    foo│         (matches atom prefix)
    "foo bar"│   (matches atom prefix)
    |}];
  show complete {|"\|};
  [%expect
    {|
    "\│
    =>
    "\nnewline"│ (matches atom prefix)
    foo│         (matches atom prefix)
    "foo bar"│   (matches atom prefix)
    |}];
  show complete {|"f|};
  [%expect
    {|
    "f│
    =>
    "\nnewline"│
    foo│         (matches atom prefix)
    "foo bar"│   (matches atom prefix)
    |}];
  show complete {|"\n|};
  [%expect
    {|
    "\n│
    =>
    "\nnewline"│ (matches atom prefix)
    foo│
    "foo bar"│
    |}]
;;

let%expect_test "ignore capitalization" =
  let open struct
    [@@@warning "-37"] (* ignore unused constructor warning *)

    type ignore_ = Foo [@@deriving sexp_grammar]
    type match_ = [ `bar ] [@@deriving sexp_grammar]
  end in
  let ignore_ = Staged.unstage (Sexp_grammar_completion.complete ignore__sexp_grammar) in
  show ignore_ "f";
  [%expect
    {|
    f│
    =>
    Foo│ (matches atom prefix)
    |}];
  show ignore_ "F";
  [%expect
    {|
    F│
    =>
    Foo│ (matches atom prefix)
    |}];
  let match_ = Staged.unstage (Sexp_grammar_completion.complete match__sexp_grammar) in
  show match_ "b";
  [%expect
    {|
    b│
    =>
    bar│ (matches atom prefix)
    |}];
  show match_ "B";
  [%expect
    {|
    B│
    =>
    bar│
    |}]
;;

let%expect_test "Any" =
  (* Regression test for a bug with the [Any] grammar. *)
  let test t =
    let s = Sexp.to_string_hum t in
    let complete =
      Staged.unstage (Sexp_grammar_completion.complete { untyped = Any "A" })
    in
    String.iteri s ~f:(fun pos _ ->
      match Prefix.of_substring s ~pos:0 ~len:pos with
      | None -> ()
      | Some (in_sexp, _) ->
        let completion = complete in_sexp in
        let error =
          match completion with
          | Ok completion ->
            (match Sexp_grammar_completion_protocol.Candidates.exhaustive completion with
             | false -> None
             | true -> Some "Expected others.")
          | Error _ -> Some "Expected suggestion."
        in
        Option.iter error ~f:(fun error ->
          print_endline (mark_positions s ~pos:[ pos ]);
          print_cr
            [%message
              error
                (completion : Sexp_grammar_completion_protocol.Candidates.t Or_error.t)]))
  in
  test [%sexp "", ""];
  [%expect {| |}]
;;

let%expect_test "comments" =
  show_sexp [%sexp_grammar: int list] ";" ~pos:0;
  [%expect {| (Ok ((candidates (Enter_list)) (exhaustive true))) |}];
  show_sexp [%sexp_grammar: int list] ";" ~pos:1;
  [%expect {| "Cannot find prefix." |}];
  show_sexp [%sexp_grammar: int list] "; " ~pos:2;
  [%expect {| "Cannot find prefix." |}]
;;

let%expect_test "inline records" =
  let open struct
    [@@@warning "-37"] (* ignore unused constructor warning *)

    type t =
      | T of
          { i : int
          ; b : bool
          }
    [@@deriving sexp_grammar]
  end in
  check_every_position t_sexp_grammar {|(T (i 1) (b true))|};
  [%expect
    {|
    No suggestions at the following locations:
    ------------------------------------------
    (T (i │1│) (b true)│)
    |}];
  quickcheck t_sexp_grammar Sexp.to_string;
  [%expect {| |}]
;;

let%expect_test "[allow_extra_fields]" =
  let open struct
    [@@@warning "-37"] (* ignore unused constructor warning *)

    module Allow = struct
      type t = { i : int } [@@deriving sexp, sexp_grammar] [@@allow_extra_fields]
    end

    module Disallow = struct
      type t = { i : int } [@@deriving sexp, sexp_grammar]
    end
  end in
  let complete grammar = Sexp_grammar_completion.complete grammar |> Staged.unstage in
  let test s =
    show (complete [%sexp_grammar: Allow.t]) s;
    let diff = Staged.unstage (diff_printer (Some [%expect.output])) in
    ignore ([%expect.output] : string);
    show (complete [%sexp_grammar: Disallow.t]) s;
    diff [%expect.output]
  in
  test "(";
  [%expect
    {|
    === DIFF HUNK ===
      (│
      =>
    -|(inexhaustive)
      ((i │) (matches atom prefix)
    |}];
  test "((";
  [%expect
    {|
    === DIFF HUNK ===
      ((│
      =>
    -|(inexhaustive)
      ((i│ (matches atom prefix)
    |}];
  test "((i";
  [%expect
    {|
    === DIFF HUNK ===
      ((i│
      =>
    -|(inexhaustive)
      ((i│ (matches atom prefix)
    |}]
;;

let%expect_test "optional record fields" =
  let open struct
    type t =
      { required : int
      ; optional : int [@default 1]
      }
    [@@deriving sexp_grammar]
  end in
  let complete = Sexp_grammar_completion.complete [%sexp_grammar: t] |> Staged.unstage in
  show complete "(";
  [%expect
    {|
    (│
    =>
    ((optional │) (matches atom prefix)
    ((required │) (matches atom prefix)
    |}];
  show complete "((";
  [%expect
    {|
    ((│
    =>
    ((optional│ (matches atom prefix)
    ((required│ (matches atom prefix)
    |}];
  show complete "((required 1)";
  [%expect
    {|
    ((required 1)│
    =>
    (inexhaustive)
    ((required 1)(optional │) (matches atom prefix)
    |}];
  show complete "((required 1)(";
  [%expect
    {|
    ((required 1)(│
    =>
    (inexhaustive)
    ((required 1)(optional│ (matches atom prefix)
    |}];
  show complete "((required 1)(optional 1)";
  [%expect
    {|
    ((required 1)(optional 1)│
    =>
    (inexhaustive)
    |}]
;;

let%expect_test "CLI completions" =
  let complete =
    foo_grammar |> Sexp_grammar_completion.command_auto_complete |> Staged.unstage
  in
  let arg_t = Core.Command.Arg_type.create ~complete Fn.id in
  let param =
    let%map_open.Core.Command (_ : _) = flag "--" (required arg_t) ~doc:"" in
    ()
  in
  let test part =
    Command_test_helpers.complete param ~args:[ "--"; part ];
    print_string
      (String.substr_replace_first
         [%expect.output]
         ~pattern:{|(command.ml.Exit_called (status 0))|}
         ~with_:"")
  in
  test {||};
  [%expect
    {|
    "\nnewline"
    foo
    "foo bar"
    |}];
  test {|f|};
  [%expect
    {|
    foo
    "foo bar"
    |}];
  test {|"\|};
  [%expect
    {|
    "\nnewline"
    foo
    "foo bar"
    |}];
  test {|"f|};
  [%expect
    {|
    foo
    "foo bar"
    |}];
  test {|"\n|};
  [%expect {| "\nnewline" |}]
;;

let%expect_test "exhaustive is incorrect for union of empty list and non-empty" =
  Sexp_grammar_completion_test_helpers.quickcheck
    ~cr:CR_someday
    [%sexp_grammar: bool option]
    Sexp.to_string_hum;
  [%expect
    {|
    ("quickcheck: test failed" (input ()))
    ("()"
      (pos 1)
      (t (
        Exhaustive_suggestions_did_not_match_input
        ((candidates (
           (Add_atom (
             (atom_signified false)
             (case_sensitivity Case_sensitive_except_first_character)
             (documentation ())))
           (Add_atom (
             (atom_signified true)
             (case_sensitivity Case_sensitive_except_first_character)
             (documentation ())))))
         (exhaustive true)))))
    |}]
;;
