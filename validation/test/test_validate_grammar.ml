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
        ("An item in list did not satisfy grammar." (
          "invalid bool" (
            reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 0)))))))) |}];
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
    Staged.unstage (Sexp_grammar.validate_sexp [%sexp_grammar: Case_insensitive.t]) sexp
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
    let validate = Sexp_grammar.validate_sexp { untyped } |> Staged.unstage in
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
    ("An item in list did not satisfy grammar." (
      "invalid bool" (
        reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 1))))) |}];
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

let%expect_test "[validate_grammar] terminates on [Any] behind [Tycon]" =
  require_valid
    (module struct
      type t = unit [@@deriving compare, quickcheck, sexp]

      let t_sexp_grammar : t Sexp_grammar.t =
        { untyped =
            Sexp_grammar.Tycon
              ("t", [], [ { tycon = "t"; tyvars = []; grammar = Any "Unit" } ])
        }
      ;;
    end);
  [%expect {| (Tycon t () (((tycon t) (tyvars ()) (grammar (Any Unit))))) |}]
;;

let%expect_test "[validate_sexp] does not explode on misbehaved [Cons]" =
  let module M = struct
    type t =
      [ `A of int
      | `B of t
      | `C of t
      ]
    [@@deriving compare, quickcheck, sexp]

    let (t_sexp_grammar : t Sexp_grammar.t) =
      { untyped =
          Tycon
            ( "t"
            , []
            , [ { tycon = "t"
                ; tyvars = []
                ; grammar =
                    Union
                      [ List
                          (Cons
                             ( Variant
                                 { case_sensitivity = Case_sensitive
                                 ; clauses =
                                     [ No_tag { name = "A"; clause_kind = Atom_clause } ]
                                 }
                             , Cons (Integer, Empty) ))
                      ; List
                          (Cons
                             ( Variant
                                 { case_sensitivity = Case_sensitive
                                 ; clauses =
                                     [ No_tag { name = "B"; clause_kind = Atom_clause } ]
                                 }
                             , Cons (Recursive ("t", []), Empty) ))
                      ; List
                          (Cons
                             ( Variant
                                 { case_sensitivity = Case_sensitive
                                 ; clauses =
                                     [ No_tag { name = "C"; clause_kind = Atom_clause } ]
                                 }
                             , Cons (Recursive ("t", []), Empty) ))
                      ]
                }
              ] )
      }
    ;;
  end
  in
  let validate = Staged.unstage (Sexp_grammar.validate_sexp M.t_sexp_grammar) in
  let example_sexp =
    Sexplib.Sexp.of_string
      {|
   (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (C (A 0))))))))))))))))))))))))
      |}
  in
  require_ok [%here] (validate example_sexp);
  [%expect {| |}]
;;

let%expect_test "[validate_grammar] does not explode on misbehaved [Many]" =
  let module M = struct
    type a_layer = [ `A | `T of t ] list
    and b_layer = [ `B | `T of t ] list

    and t =
      [ `A_layer of a_layer
      | `B_layer of b_layer
      ]
    [@@deriving compare, quickcheck]

    let rec sexp_of_t = function
      | `A_layer a_layer -> sexp_of_a_layer a_layer
      | `B_layer b_layer -> sexp_of_b_layer b_layer

    and sexp_of_a_layer a_layer =
      List.sexp_of_t
        (function
          | `A -> Sexp.Atom "A"
          | `T t -> sexp_of_t t)
        a_layer

    and sexp_of_b_layer b_layer =
      List.sexp_of_t
        (function
          | `B -> Sexp.Atom "B"
          | `T t -> sexp_of_t t)
        b_layer
    ;;

    let _ = sexp_of_t

    let rec t_of_sexp : Sexp.t -> t = function
      | Atom a -> raise_s [%message "unexpected atom" a]
      | List elts as sexp ->
        if List.mem elts (Sexp.Atom "B") ~equal:Sexp.equal
        then `B_layer (b_layer_of_sexp sexp)
        else `A_layer (a_layer_of_sexp sexp)

    and a_layer_of_sexp sexp =
      List.t_of_sexp
        (function
          | Atom "A" -> `A
          | List _ as sexp -> `T (t_of_sexp sexp)
          | Atom a -> raise_s [%message "wrong atom" a])
        sexp

    and b_layer_of_sexp sexp =
      List.t_of_sexp
        (function
          | Atom "B" -> `B
          | List _ as sexp -> `T (t_of_sexp sexp)
          | Atom a -> raise_s [%message "wrong atom" a])
        sexp
    ;;

    let _ = t_of_sexp

    let defns : Sexp_grammar.defn list =
      [ { tycon = "t"
        ; tyvars = []
        ; grammar = Union [ Recursive ("a_layer", []); Recursive ("b_layer", []) ]
        }
      ; { tycon = "a_layer"
        ; tyvars = []
        ; grammar =
            List
              (Many
                 (Union
                    [ Variant
                        { case_sensitivity = Case_sensitive
                        ; clauses = [ No_tag { name = "A"; clause_kind = Atom_clause } ]
                        }
                    ; Recursive ("t", [])
                    ]))
        }
      ; { tycon = "b_layer"
        ; tyvars = []
        ; grammar =
            List
              (Many
                 (Union
                    [ Variant
                        { case_sensitivity = Case_sensitive
                        ; clauses = [ No_tag { name = "B"; clause_kind = Atom_clause } ]
                        }
                    ; Recursive ("t", [])
                    ]))
        }
      ]
    ;;

    let t_sexp_grammar : t Sexp_grammar.t = { untyped = Tycon ("t", [], defns) }
  end
  in
  let validate = Staged.unstage (Sexp_grammar.validate_sexp M.t_sexp_grammar) in
  let example_sexp =
    Sexplib.Sexp.of_string
      {|
   (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A (A))))))))))))))))))))))))
      |}
  in
  require_ok [%here] (validate example_sexp);
  [%expect {| |}]
;;

let%expect_test "[validate_sexp] does not explode on recomputed left side of infix \
                 operator"
  =
  let module M = struct
    type t =
      [ `Int of int
      | `Add of t * t
      | `Sub of t * t
      | `Mul of t * t
      | `Div of t * t
      ]
    [@@deriving compare, quickcheck]

    let rec sexp_of_t = function
      | `Int i -> sexp_of_int i
      | `Add (t1, t2) -> Sexp.List [ sexp_of_t t1; Sexp.Atom "+"; sexp_of_t t2 ]
      | `Sub (t1, t2) -> Sexp.List [ sexp_of_t t1; Sexp.Atom "-"; sexp_of_t t2 ]
      | `Mul (t1, t2) -> Sexp.List [ sexp_of_t t1; Sexp.Atom "*"; sexp_of_t t2 ]
      | `Div (t1, t2) -> Sexp.List [ sexp_of_t t1; Sexp.Atom "/"; sexp_of_t t2 ]
    ;;

    let _ = sexp_of_t

    let rec t_of_sexp : Sexp.t -> t = function
      | Atom _ as sexp -> `Int (int_of_sexp sexp)
      | List [ left; Atom "+"; right ] -> `Add (t_of_sexp left, t_of_sexp right)
      | List [ left; Atom "-"; right ] -> `Sub (t_of_sexp left, t_of_sexp right)
      | List [ left; Atom "*"; right ] -> `Mul (t_of_sexp left, t_of_sexp right)
      | List [ left; Atom "/"; right ] -> `Div (t_of_sexp left, t_of_sexp right)
      | List _ as sexp -> raise_s [%message "unexpected list shape" (sexp : Sexp.t)]
    ;;

    let _ = t_of_sexp

    let (t_sexp_grammar : t Sexp_grammar.t) =
      let binop op : Sexp_grammar.grammar =
        List
          (Cons
             ( Recursive ("t", [])
             , Cons
                 ( Variant
                     { case_sensitivity = Case_sensitive
                     ; clauses = [ No_tag { name = op; clause_kind = Atom_clause } ]
                     }
                 , Cons (Recursive ("t", []), Empty) ) ))
      in
      { untyped =
          Tycon
            ( "t"
            , []
            , [ { tycon = "t"
                ; tyvars = []
                ; grammar = Union [ Integer; binop "+"; binop "-"; binop "*"; binop "/" ]
                }
              ] )
      }
    ;;
  end
  in
  let validate = Staged.unstage (Sexp_grammar.validate_sexp M.t_sexp_grammar) in
  let depth = 15 in
  let example_sexp =
    List.fold (List.range 0 depth) ~init:(Sexp.Atom "1") ~f:(fun sexp _ ->
      Sexp.List [ sexp; Atom "*"; Atom "1" ])
  in
  require_ok [%here] (validate example_sexp);
  [%expect {| |}]
;;
