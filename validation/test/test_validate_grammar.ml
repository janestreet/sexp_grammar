open! Base
open! Import

let require_valid m =
  match Sexp_grammar_validation.validate_grammar m with
  | Ok () -> ()
  | Error error -> print_cr [%here] [%message "Invalid grammar." ~_:(error : Error.t)]
;;

let require_invalid m =
  match Sexp_grammar_validation.validate_grammar m with
  | Ok () -> print_cr [%here] [%message "Unexpectedly valid grammar."]
  | Error error -> print_s [%sexp (error : Error.t)]
;;

let%expect_test "valid" =
  require_valid
    (module struct
      type t = int [@@deriving compare, quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| Integer |}];
  require_valid
    (module struct
      type t = string [@@deriving compare, quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| String |}];
  require_valid
    (module struct
      type t = bool [@@deriving compare, quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| Bool |}];
  require_valid
    (module struct
      type t = float [@@deriving compare, quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| Float |}];
  require_valid
    (module struct
      type t = int list [@@deriving compare, quickcheck, sexp, sexp_grammar]
    end);
  [%expect {| (List (Many Integer)) |}];
  return ()
;;

let%expect_test "[sexp_of_t] output disobeys [t_sexp_grammar]" =
  require_invalid
    (module struct
      type t = bool list [@@deriving compare, quickcheck, sexp_grammar]

      include
        Sexpable.Of_sexpable
          (struct
            type t = int list [@@deriving sexp]
          end)
          (struct
            type t = bool list

            let to_sexpable = List.map ~f:Bool.to_int
            let of_sexpable = List.map ~f:(Int.( <> ) 0)
          end)
    end);
  [%expect
    {|
    (List (Many Bool))
    ("Base_quickcheck.Test.run: test failed"
      (input (0 1))
      (error (
        "[t_sexp_grammar] rejects sexp from [sexp_of_t]"
        ("Some items in list did not satisfy grammar."
         (("invalid bool" (
            reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 0))))
          ("invalid bool" (
            reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 1))))))))) |}];
  require_invalid
    (module struct
      type t = int [@@deriving compare, quickcheck, sexp_grammar]

      include
        Sexpable.Of_sexpable
          (struct
            type t = [ `foo of int ] [@@deriving sexp]
          end)
          (struct
            type t = int

            let to_sexpable t = `foo t
            let of_sexpable (`foo t) = t
          end)
    end);
  [%expect
    {|
    Integer
    ("Base_quickcheck.Test.run: test failed"
      (input (foo 76753))
      (error (
        "[t_sexp_grammar] rejects sexp from [sexp_of_t]"
        ("invalid integer" (
          reason (
            Of_sexp_error
            "Sexpable.Of_stringable.t_of_sexp expected an atom, but got a list"
            (invalid_sexp (foo 76753)))))))) |}];
  return ()
;;

let%expect_test "[t_of_sexp] accepts input disobeying [t_sexp_grammar]" =
  let module Without_default = struct
    type t = { foo : bool } [@@deriving sexp_grammar]
  end
  in
  let module M = struct
    type t = Without_default.t = { foo : bool [@sexp.default true] }
    [@@deriving compare, quickcheck, sexp]

    let t_sexp_grammar = [%sexp_grammar: Without_default.t]
  end
  in
  require_invalid (module M);
  [%expect
    {|
    (List
     (Fields
      ((allow_extra_fields false)
       (fields ((No_tag ((name foo) (required true) (args (Cons Bool Empty)))))))))
    ("Base_quickcheck.Test.run: test failed"
      (input (() ("missing record field" (field_name foo))))
      (error (
        "[t_sexp_grammar] rejects sexp that [t_of_sexp] accepts"
        (value ((foo true)))
        (error ("missing record field" (field_name foo)))))) |}]
;;

let%expect_test "[t_of_sexp] accepts input disobeying [t_sexp_grammar] by string case" =
  let module Case_insensitive = struct
    type t = One [@@deriving sexp, sexp_grammar, quickcheck]

    let t_of_sexp : Sexp.t -> t = function
      | List _ as sexp -> t_of_sexp sexp
      | Atom s ->
        (match String.lowercase s with
         | "one" -> One
         | _ -> t_of_sexp (Atom s))
    ;;

    let t_sexp_grammar : t Sexplib.Sexp_grammar.t =
      { untyped =
          Variant
            { case_sensitivity = Case_sensitive_except_first_character
            ; clauses = [ No_tag { name = "one"; clause_kind = Atom_clause } ]
            }
      }
    ;;
  end
  in
  let sexp = [%sexp "ONE"] in
  let test_t_of_sexp sexp =
    Or_error.try_with (fun () -> [%of_sexp: Case_insensitive.t] sexp)
  in
  require_ok [%here] (test_t_of_sexp sexp);
  [%expect {| |}];
  let test_validate_grammar sexp =
    Staged.unstage
      (Sexp_grammar_validation.validate_sexp [%sexp_grammar: Case_insensitive.t])
      sexp
  in
  require_error [%here] [%sexp_of: unit] ~print_error:true (test_validate_grammar sexp);
  [%expect
    {|
    ("invalid variant; unrecognized name"
      (clause_name ONE)
      (case_sensitivity Case_sensitive_except_first_character)
      (recognized (one))) |}];
  (* The grammar should be invalid, as it wants case sensitivity *)
  require_invalid (module Case_insensitive);
  [%expect
    {|
(Variant
 ((case_sensitivity Case_sensitive_except_first_character)
  (clauses ((No_tag ((name one) (clause_kind Atom_clause)))))))
("Base_quickcheck.Test.run: test failed"
  (input (
    onE (
      "invalid variant; unrecognized name"
      (clause_name onE)
      (case_sensitivity Case_sensitive_except_first_character)
      (recognized (one)))))
  (error (
    "[t_sexp_grammar] rejects sexp that [t_of_sexp] accepts"
    (value One)
    (error (
      "invalid variant; unrecognized name"
      (clause_name onE)
      (case_sensitivity Case_sensitive_except_first_character)
      (recognized (one))))))) |}]
;;

let%expect_test "[spot_check_grammar]" =
  let make_spot_check grammar of_sexp =
    Sexp_grammar_validation.spot_check_grammar (Sexp_grammar.coerce grammar) of_sexp
    |> Staged.unstage
  in
  let test_ok sexp =
    require_ok [%here] (make_spot_check [%sexp_grammar: unit] [%of_sexp: unit] sexp)
  in
  test_ok [%sexp ()];
  [%expect {| |}];
  test_ok [%sexp 1];
  [%expect {| |}];
  let test_error sexp =
    make_spot_check [%sexp_grammar: unit] [%of_sexp: int] sexp
    |> require_error [%here] [%sexp_of: unit] ~print_error:true
  in
  test_error [%sexp ()];
  [%expect
    {|
    ("grammar disagrees with [t_of_sexp] as to whether this sexp is valid."
     (sexp ())
     (accepted_by_grammar (Ok ()))
     (accepted_by_t_of_sexp (
       Error (Of_sexp_error "int_of_sexp: atom needed" (invalid_sexp ()))))) |}];
  test_error [%sexp 1];
  [%expect
    {|
    ("grammar disagrees with [t_of_sexp] as to whether this sexp is valid."
     (sexp 1)
     (accepted_by_grammar (Error ("expected a list" (sexp 1))))
     (accepted_by_t_of_sexp (Ok ()))) |}]
;;

let%expect_test "validation failure messages" =
  let show_error untyped sexp =
    let validate = Sexp_grammar_validation.validate_sexp { untyped } |> Staged.unstage in
    require_error [%here] [%sexp_of: unit] (validate sexp) ~print_error:true
  in
  show_error Bool [%sexp 1];
  [%expect
    {|
    ("invalid bool" (
      reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 1)))) |}];
  show_error Char [%sexp 11];
  [%expect
    {|
    ("invalid char" (
      reason (
        Of_sexp_error
        "char_of_sexp: atom string must contain one character only"
        (invalid_sexp 11)))) |}];
  show_error Integer [%sexp A];
  [%expect
    {|
    ("invalid integer" (
      reason (
        Of_sexp_error
        (Invalid_argument "Z.of_substring_base: invalid digit")
        (invalid_sexp A)))) |}];
  show_error Float [%sexp A];
  [%expect
    {|
    ("invalid float" (
      reason (
        Of_sexp_error "float_of_sexp: (Failure float_of_string)" (invalid_sexp A)))) |}];
  show_error String [%sexp { foo = 1 }];
  [%expect
    {|
    ("invalid string" (
      reason (
        Of_sexp_error "string_of_sexp: atom needed" (invalid_sexp ((foo 1)))))) |}];
  show_error (Option Bool) [%sexp A];
  [%expect {| ("expected an option" (sexp A)) |}];
  show_error (List (Cons (Bool, Empty))) [%sexp A];
  [%expect {| ("expected a list" (sexp A)) |}];
  show_error (List (Cons (Bool, Empty))) [%sexp [ A ]];
  [%expect
    {|
    ("invalid bool" (
      reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp A)))) |}];
  show_error (List (Cons (Bool, Empty))) [%sexp [ true; A ]];
  [%expect {| ("too many sexps in list" (remaining_sexps (A))) |}];
  show_error (List (Cons (Bool, Empty))) [%sexp []];
  [%expect {| "too few sexps in list" |}];
  show_error (List (Many Bool)) [%sexp [ 1; 2 ]];
  [%expect
    {|
    ("Some items in list did not satisfy grammar."
     (("invalid bool" (
        reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 1))))
      ("invalid bool" (
        reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 2)))))) |}];
  show_error (Union [ Char; Bool ]) [%sexp 11];
  [%expect
    {|
    ("expected union of several grammars, but none were satisfied."
     (("invalid char" (
        reason (
          Of_sexp_error
          "char_of_sexp: atom string must contain one character only"
          (invalid_sexp 11))))
      ("invalid bool" (
        reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 11)))))) |}];
  let module M = struct
    type t = { foo : int } [@@deriving sexp_grammar]

    let show_error = show_error t_sexp_grammar.untyped
  end
  in
  M.show_error [%sexp A 1];
  [%expect {| ("invalid record field; expected a list with a leading atom" (sexp A)) |}];
  M.show_error [%sexp { foo = 1; foo = 1 }];
  [%expect {| ("duplicate record field" (field_name foo)) |}];
  M.show_error [%sexp { bar = 1 }];
  [%expect
    {|
    ("unrecognized record field"
      (field_name bar)
      (recognized (foo))
      (sexp (bar 1))) |}];
  M.show_error [%sexp []];
  [%expect {| ("missing record field" (field_name foo)) |}];
  let module M = struct
    type t =
      | A
      | B of int
    [@@deriving sexp_grammar] [@@warning "-37"]

    let show_error = show_error t_sexp_grammar.untyped
  end
  in
  M.show_error [%sexp C];
  [%expect
    {|
    ("invalid variant; unrecognized name"
      (clause_name C)
      (case_sensitivity Case_sensitive_except_first_character)
      (recognized (A B))) |}];
  M.show_error [%sexp ()];
  [%expect
    {| ("invalid variant; expected an atom or a list with a leading atom" (sexp ())) |}];
  M.show_error [%sexp B];
  [%expect {| ("invalid \"B\" variant; expected a list but got an atom" (sexp B)) |}];
  M.show_error [%sexp [ A ]];
  [%expect {| ("invalid \"A\" variant; expected an atom but got a list" (sexp (A))) |}]
;;
