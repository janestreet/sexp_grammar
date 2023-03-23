open! Core
open Base_quickcheck
open Generator.Let_syntax

let rec without_tag_list grammar =
  match (grammar : 'a Sexp_grammar.with_tag_list) with
  | No_tag grammar -> grammar
  | Tag { key = _; value = _; grammar } -> without_tag_list grammar
;;

module Generate = struct
  module type S = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  let of_type (module M : S) =
    [%quickcheck.generator: M.t] |> Generator.map ~f:[%sexp_of: M.t]
  ;;

  let name_examples ~case_sensitivity string =
    let%map.List change_case =
      match (case_sensitivity : Sexp_grammar.case_sensitivity) with
      | Case_insensitive ->
        [ String.capitalize; String.uncapitalize; String.lowercase; String.uppercase ]
      | Case_sensitive -> [ Fn.id ]
      | Case_sensitive_except_first_character ->
        [ String.capitalize; String.uncapitalize ]
    in
    change_case string
  ;;

  let rec on_grammar grammar =
    match (grammar : Sexp_grammar.grammar) with
    | Any _ -> Generator.sexp
    | Bool -> of_type (module Bool)
    | Char -> of_type (module Char)
    | Integer -> of_type (module Bigint)
    | Float -> of_type (module Float)
    | String -> of_type (module String)
    | Tagged { key = _; value = _; grammar } -> on_grammar grammar
    | Option grammar ->
      let value_gen = on_grammar grammar in
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
    | Union grammars -> Generator.union (List.map grammars ~f:on_grammar)
    | Lazy lazy_grammar -> Generator.of_lazy (Lazy.map lazy_grammar ~f:on_grammar)
    | List list_grammar ->
      let list_gen = on_list_grammar list_grammar in
      Generator.map list_gen ~f:(fun list -> Sexp.List list)
    | Variant { case_sensitivity; clauses } ->
      let clauses = List.map ~f:without_tag_list clauses in
      List.map clauses ~f:(fun ({ name; clause_kind } : Sexp_grammar.clause) ->
        let%bind name = Generator.of_list (name_examples ~case_sensitivity name) in
        match clause_kind with
        | Atom_clause -> return (Sexp.Atom name)
        | List_clause { args } ->
          let%map sexps = on_list_grammar args in
          Sexp.List (Sexp.Atom name :: sexps))
      |> Generator.union
    | Tycon _ ->
      let lazy_grammar = lazy (Sexp_grammar.unroll_tycon_untyped grammar) in
      Generator.of_lazy (Lazy.map lazy_grammar ~f:on_grammar)
    | Tyvar _ | Recursive _ ->
      raise_s
        [%message
          "Unexpected [Tyvar] or [Tycon] after [unroll_tycon]"
            ~grammar:(grammar : Sexp_grammar.grammar)]

  and on_list_grammar list_grammar =
    match list_grammar with
    | Many grammar -> Generator.list (on_grammar grammar)
    | Empty -> Generator.return []
    | Cons (head, tail) ->
      let%map head = on_grammar head
      and tail = on_list_grammar tail in
      head :: tail
    | Fields { allow_extra_fields; fields } ->
      let fields = List.map ~f:without_tag_list fields in
      let field_gens =
        List.map fields ~f:(fun { name; required; args } ->
          let list_gen = on_list_grammar args in
          let required_gen =
            let%map sexps = list_gen in
            [ Sexp.List (Sexp.Atom name :: sexps) ]
          in
          if required
          then required_gen
          else Generator.union [ Generator.return []; required_gen ])
      in
      let extra_fields_gen =
        match allow_extra_fields with
        | false -> Generator.return []
        | true ->
          let%bind names =
            let field_set =
              fields
              |> List.map ~f:(fun { name; _ } -> name)
              |> Set.of_list (module String)
            in
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
end

let create ({ untyped } : _ Sexp_grammar.t) = Generate.on_grammar untyped
