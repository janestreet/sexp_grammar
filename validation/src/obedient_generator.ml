open! Core
open Base_quickcheck
open Generator.Let_syntax

module Generate_callbacks = struct
  type t = Sexp.t Generator.t

  let any _ = Generator.sexp

  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  let of_type (module M : S) =
    [%quickcheck.generator: M.t] |> Generator.map ~f:[%sexp_of: M.t]
  ;;

  let bool = of_type (module Bool)
  let char = of_type (module Char)
  let integer = of_type (module Bigint)
  let float = of_type (module Float)
  let string = of_type (module String)

  let name_examples ~name_kind string =
    match (name_kind : Sexp_grammar.name_kind) with
    | Any_case -> [ string ]
    | Capitalized -> [ String.capitalize string; String.uncapitalize string ]
  ;;

  let option value_gen =
    let old = !Sexplib0.Sexp_conv.write_old_option_format in
    [ true, 1., Generator.return (Sexp.Atom "none")
    ; true, 1., Generator.return (Sexp.Atom "None")
    ; old, 2., Generator.return (Sexp.List [])
    ; true, 1., Generator.map value_gen ~f:(fun sexp -> Sexp.List [ Atom "some"; sexp ])
    ; true, 1., Generator.map value_gen ~f:(fun sexp -> Sexp.List [ Atom "Some"; sexp ])
    ; old, 2., Generator.map value_gen ~f:(fun sexp -> Sexp.List [ sexp ])
    ]
    |> List.filter_map ~f:(fun (keep, weight, gen) ->
      if keep then Some (weight, gen) else None)
    |> Generator.weighted_union
  ;;

  let union = Generator.union
  let lazy_ = Generator.of_lazy
  let of_lazy_recursive = lazy_

  type list_t = Sexp.t list Generator.t

  let list list_gen = Generator.map list_gen ~f:(fun list -> Sexp.List list)
  let many = Generator.list
  let empty = Generator.return []

  let cons head tail =
    let%map head = head
    and tail = tail in
    head :: tail
  ;;

  let record fields ~allow_extra_fields =
    let field_gens =
      List.map fields ~f:(fun (field_name, field) ->
        let required_gen list_gen =
          let%map sexps = list_gen in
          [ Sexp.List (Sexp.Atom field_name :: sexps) ]
        in
        match (field : list_t Sexp_grammar.Field.t) with
        | Required list_gen -> required_gen list_gen
        | Optional list_gen ->
          Generator.union [ Generator.return []; required_gen list_gen ])
    in
    let extra_fields_gen =
      match allow_extra_fields with
      | false -> Generator.return []
      | true ->
        let%bind names =
          let field_set = fields |> List.map ~f:fst |> Set.of_list (module String) in
          Generator.string_non_empty_of Generator.char_alpha
          |> Generator.filter ~f:(fun name -> not (Set.mem field_set name))
          |> Generator.list
        in
        List.map names ~f:(fun name ->
          let%map sexps = Generator.list Generator.sexp in
          Sexp.List (Sexp.Atom name :: sexps))
        |> Generator.all
    in
    let%bind sexps = Generator.all (extra_fields_gen :: field_gens) in
    List.concat sexps |> Generator.list_permutations
  ;;

  let variant clauses ~name_kind =
    List.map clauses ~f:(fun (clause_name, maybe_list_gen) ->
      let%bind name = Generator.of_list (name_examples ~name_kind clause_name) in
      match maybe_list_gen with
      | None -> return (Sexp.Atom name)
      | Some list_gen ->
        let%map sexps = list_gen in
        Sexp.List (Sexp.Atom name :: sexps))
    |> Generator.union
  ;;
end

module Generate = Sexp_grammar.Fold_recursive (Generate_callbacks)

let create = Generate.of_typed_grammar_exn
