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
        ("invalid bool" (
          reason (Of_sexp_error "bool_of_sexp: unknown string" (invalid_sexp 0))))))) |}];
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
       (fields (((name foo) (required true) (args (Cons Bool Empty))))))))
    ("Base_quickcheck.Test.run: test failed"
      (input (() ("missing record field" (field_name foo))))
      (error (
        "[t_sexp_grammar] rejects sexp that [t_of_sexp] accepts"
        (value ((foo true)))
        (error ("missing record field" (field_name foo)))))) |}]
;;
