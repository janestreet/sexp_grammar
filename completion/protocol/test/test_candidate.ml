open! Core
open! Import

module _ : module type of Candidate = struct
  let insert_to_left = Candidate.insert_to_left
  let insert_to_right = Candidate.insert_to_right

  (* Helpers *)
  open struct
    let mark = "│"

    let show t =
      print_endline [%string {|%{insert_to_left t}%{mark}%{insert_to_right t}|}]
    ;;
  end

  let%expect_test "[insert_to_left], [insert_to_right]" =
    let atoms_to_add =
      let%map.List case_sensitivity = Candidate.Case_sensitivity.all
      and atom_signified = [ "foo"; "Foo"; "FOO" ] in
      ({ atom_signified; case_sensitivity; documentation = [] } : Candidate.Atom_to_add.t)
    in
    List.iter atoms_to_add ~f:(fun atom_to_add ->
      print_endline "";
      print_s [%sexp (atom_to_add : Candidate.Atom_to_add.t)];
      show (Add_atom atom_to_add));
    [%expect
      {|
      ((atom_signified foo) (case_sensitivity Case_insensitive) (documentation ()))
      foo│

      ((atom_signified Foo) (case_sensitivity Case_insensitive) (documentation ()))
      Foo│

      ((atom_signified FOO) (case_sensitivity Case_insensitive) (documentation ()))
      FOO│

      ((atom_signified foo) (case_sensitivity Case_sensitive) (documentation ()))
      foo│

      ((atom_signified Foo) (case_sensitivity Case_sensitive) (documentation ()))
      Foo│

      ((atom_signified FOO) (case_sensitivity Case_sensitive) (documentation ()))
      FOO│

      ((atom_signified foo)
       (case_sensitivity Case_sensitive_except_first_character) (documentation ()))
      foo│

      ((atom_signified Foo)
       (case_sensitivity Case_sensitive_except_first_character) (documentation ()))
      Foo│

      ((atom_signified FOO)
       (case_sensitivity Case_sensitive_except_first_character) (documentation ()))
      FOO│
      |}];
    (* quoting *)
    show
      (Add_atom
         { atom_signified = "foo bar"
         ; case_sensitivity = Case_sensitive
         ; documentation = []
         });
    [%expect {| "foo bar"│ |}];
    (* lists *)
    show Enter_list;
    [%expect {| (│) |}];
    show
      (Enter_list_and_add_atom
         { atom_signified = "foo"; case_sensitivity = Case_sensitive; documentation = [] });
    [%expect {| (foo │) |}]
  ;;

  let matches_atom_prefix = Candidate.matches_atom_prefix

  let%expect_test "[matches_atom_prefix]" =
    let get_atom_prefix s =
      let len = String.length s in
      match Parsexp_prefix.Sexp_prefix.of_substring s ~pos:0 ~len with
      | None | Some (_ :: _, _) | Some ([], In_list _) ->
        raise_s [%message "Not an atom prefix." s]
      | Some ([], Hole atom_prefix) -> atom_prefix
    in
    let atom s ~case_sensitivity : Candidate.Atom_to_add.t =
      let atom_signified =
        let case_sensitivity =
          Enum.to_string_hum (module Candidate.Case_sensitivity) case_sensitivity
        in
        [%string {|%{s}-%{case_sensitivity}|}]
      in
      { atom_signified; case_sensitivity; documentation = [] }
    in
    let candidates : Candidate.t list =
      [ Add_atom (atom "foo" ~case_sensitivity:Case_sensitive_except_first_character)
      ; Add_atom (atom "foo" ~case_sensitivity:Case_sensitive)
      ; Add_atom (atom "bar" ~case_sensitivity:Case_sensitive)
      ; Add_atom (atom "baz" ~case_sensitivity:Case_insensitive)
      ; Enter_list
      ; Enter_list_and_add_atom (atom "qux" ~case_sensitivity:Case_sensitive)
      ; Enter_list_and_add_atom (atom "qux" ~case_sensitivity:Case_insensitive)
      ]
    in
    let test prefix =
      let atom_prefix = get_atom_prefix prefix in
      candidates
      |> List.filter ~f:(fun c -> matches_atom_prefix c atom_prefix)
      |> List.iter ~f:show
    in
    test "";
    [%expect
      {|
      foo-case-sensitive-except-first-character│
      foo-case-sensitive│
      bar-case-sensitive│
      baz-case-insensitive│
      (│)
      (qux-case-sensitive │)
      (qux-case-insensitive │)
      |}];
    test "b";
    [%expect
      {|
      bar-case-sensitive│
      baz-case-insensitive│
      |}];
    test "f";
    [%expect
      {|
      foo-case-sensitive-except-first-character│
      foo-case-sensitive│
      |}];
    test "F";
    [%expect {| foo-case-sensitive-except-first-character│ |}];
    test "FO";
    [%expect {| |}];
    test "B";
    [%expect {| baz-case-insensitive│ |}];
    test "BA";
    [%expect {| baz-case-insensitive│ |}];
    test "bA";
    [%expect {| baz-case-insensitive│ |}]
  ;;

  let documentation = Candidate.documentation

  let%expect_test "[documentation]" =
    let atom_to_add documentation : Candidate.Atom_to_add.t =
      { atom_signified = "foo"; case_sensitivity = Case_sensitive; documentation }
    in
    let test t = List.iter (documentation t) ~f:print_endline in
    test Enter_list;
    [%expect {| |}];
    test (Enter_list_and_add_atom (atom_to_add [ "doc"; "moar docca" ]));
    [%expect
      {|
      doc
      moar docca
      |}];
    test (Add_atom (atom_to_add [ "doc"; "moar docca" ]));
    [%expect
      {|
      doc
      moar docca
      |}];
    ()
  ;;

  let compare = Candidate.compare

  let%expect_test "[compare]" =
    let add_atom atom_signified documentation : Candidate.t =
      Add_atom { atom_signified; case_sensitivity = Case_sensitive; documentation }
    in
    let test compare =
      [ add_atom "Foo" [ "Some foo doc" ]
      ; add_atom "Bar" [ "The bar doc" ]
      ; add_atom "Foo" [ "Another foo doc" ]
      ]
      |> List.sort ~compare
      |> [%sexp_of: Candidate.t list]
      |> print_s
    in
    test compare;
    [%expect
      {|
      ((Add_atom
        ((atom_signified Bar) (case_sensitivity Case_sensitive)
         (documentation ("The bar doc"))))
       (Add_atom
        ((atom_signified Foo) (case_sensitivity Case_sensitive)
         (documentation ("Another foo doc"))))
       (Add_atom
        ((atom_signified Foo) (case_sensitivity Case_sensitive)
         (documentation ("Some foo doc")))))
      |}]
  ;;

  (* deliberately not tested *)
  include (
    Candidate :
    sig
      module Atom_to_add = Candidate.Atom_to_add
      module Case_sensitivity = Candidate.Case_sensitivity

      type t = Candidate.t =
        | Add_atom of Atom_to_add.t
        | Enter_list
        | Enter_list_and_add_atom of Atom_to_add.t
      [@@deriving sexp_of]

      include
        Comparator.S
        with type t := t
        with type comparator_witness = Candidate.comparator_witness

      module Stable = Candidate.Stable
      module Unstable = Candidate.Unstable
    end)
end
