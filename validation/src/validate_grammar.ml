open! Core
open! Expect_test_helpers_base
open Or_error.Let_syntax
include Validate_grammar_intf

let show_grammar (module M : With_grammar) =
  Ref.set_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
    print_s [%sexp ([%sexp_grammar: M.t] : _ Sexp_grammar.t)])
;;

let validate_acceptance ?config (module M : S) =
  let validate = Staged.unstage (Validate_sexp.validate M.t_sexp_grammar) in
  Base_quickcheck.Test.run
    ?config
    (module struct
      type t = Sexp.t [@@deriving sexp_of]

      let quickcheck_generator =
        Base_quickcheck.Generator.map M.quickcheck_generator ~f:M.sexp_of_t
      ;;

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end)
    ~f:(fun sexp ->
      match validate sexp with
      | Ok () -> Ok ()
      | Error error ->
        Or_error.error_s
          [%message "[t_sexp_grammar] rejects sexp from [sexp_of_t]" ~_:(error : Error.t)])
;;

let validate_rejection ?config (module M : S) =
  Base_quickcheck.Test.run
    ?config
    (module struct
      type t = Sexp.t * Error.t [@@deriving sexp_of]

      let quickcheck_generator = Disobedient_generator.create M.t_sexp_grammar
      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end)
    ~f:(fun (sexp, error) ->
      match M.t_of_sexp sexp with
      | exception (_ : exn) -> Ok ()
      | value ->
        Or_error.error_s
          [%message
            "[t_sexp_grammar] rejects sexp that [t_of_sexp] accepts"
              (value : M.t)
              (error : Error.t)])
;;

module Known_to_accept_all_sexps_callbacks = struct
  type t = bool
  type list_t = unit

  let any _ = true
  let bool = false
  let char = false
  let integer = false
  let float = false
  let string = false
  let option _ = false
  let union list = List.exists list ~f:Fn.id
  let list () = false
  let empty = ()
  let cons _ _ = ()
  let many _ = ()
  let record _ ~allow_extra_fields:_ = ()
  let variant _ ~name_kind:_ = false
  let tyvar _ = false
  let tycon _ ~params:_ = false
  let recursive _ ~defns:_ = false
  let lazy_ = Lazy.force
end

module Known_to_accept_all_sexps =
  Sexp_grammar.Fold_nonrecursive (Known_to_accept_all_sexps_callbacks)

let validate_grammar ?test_count (module M : S) =
  let config =
    let%map.Option test_count = test_count in
    { Base_quickcheck.Test.default_config with test_count }
  in
  show_grammar (module M);
  let%bind () = validate_acceptance ?config (module M) in
  match Known_to_accept_all_sexps.of_typed_grammar M.t_sexp_grammar with
  | false -> validate_rejection ?config (module M)
  | true -> Ok ()
;;

module A = struct
  type t = Sexp.t [@@deriving quickcheck, sexp]

  let (t_sexp_grammar : t Sexp_grammar.t) = { untyped = Any "A" }
end

let validate_grammar_poly1 ?test_count (module M : S1) =
  validate_grammar
    ?test_count
    (module struct
      type t = A.t M.t [@@deriving quickcheck, sexp, sexp_grammar]
    end)
;;
