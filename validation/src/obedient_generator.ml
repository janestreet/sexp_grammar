open! Core
open Base_quickcheck
open Generator.Let_syntax

let tag_prefix = "sexp_grammar.generator"
let recursive_tycon_tag = tag_prefix ^ ".tycon"

let rec without_tag_list grammar =
  match (grammar : 'a Sexp_grammar.with_tag_list) with
  | No_tag grammar -> grammar
  | Tag { key = _; value = _; grammar } -> without_tag_list grammar
;;

module Min_recursive_calls = struct
  let rec on_grammar grammar =
    match (grammar : Sexp_grammar.grammar) with
    | Any _ | Bool | Char | Integer | Float | String -> 0
    | Tagged { key; value = _; grammar = _ } when String.equal key recursive_tycon_tag ->
      1
    | Tagged { key = _; value = _; grammar } -> on_grammar grammar
    | Option grammar -> on_grammar grammar
    | Union grammars ->
      List.map grammars ~f:on_grammar
      |> List.min_elt ~compare:[%compare: int]
      |> Option.value ~default:0
    | Lazy lazy_grammar -> on_grammar (Lazy.force lazy_grammar)
    | List list_grammar -> on_list_grammar list_grammar
    | Variant { case_sensitivity = _; clauses } ->
      let clauses = List.map ~f:without_tag_list clauses in
      List.map clauses ~f:(fun ({ name = _; clause_kind } : Sexp_grammar.clause) ->
        match clause_kind with
        | Atom_clause -> 0
        | List_clause { args } -> on_list_grammar args)
      |> List.min_elt ~compare:[%compare: int]
      |> Option.value ~default:0
    | Tycon (_, args, _) -> List.sum (module Int) args ~f:on_grammar
    | Tyvar _ -> 0
    | Recursive _ -> 1

  and on_list_grammar list_grammar =
    match (list_grammar : Sexp_grammar.list_grammar) with
    | Empty -> 0
    | Cons (head, tail) -> on_grammar head + on_list_grammar tail
    | Many _ -> 0
    | Fields { allow_extra_fields = _; fields } ->
      let fields = List.map fields ~f:without_tag_list in
      List.sum
        (module Int)
        fields
        ~f:(fun { name = _; required = _; args } -> on_list_grammar args)
  ;;
end

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

  let recursive_union (generators : (Sexp.t Generator.t * int) list) =
    let%bind size = Generator.size in
    let gens =
      if size = 0
      then (
        match
          List.filter_map generators ~f:(fun (gen, min_recursive_calls) ->
            Option.some_if (min_recursive_calls = 0) gen)
        with
        | _ :: _ as nonempty -> nonempty
        | [] -> List.map generators ~f:fst)
      else
        List.map generators ~f:(fun (gen, min_recursive_calls) ->
          match min_recursive_calls with
          | 0 -> gen
          | 1 -> Generator.with_size gen ~size:(size - 1)
          | n -> Generator.with_size gen ~size:(size / n))
    in
    Generator.union gens
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
    | Union grammars ->
      recursive_union
        (List.map grammars ~f:(fun grammar ->
           on_grammar grammar, Min_recursive_calls.on_grammar grammar))
    | Lazy lazy_grammar -> Generator.of_lazy (Lazy.map lazy_grammar ~f:on_grammar)
    | List list_grammar ->
      let list_gen = on_list_grammar list_grammar in
      Generator.map list_gen ~f:(fun list -> Sexp.List list)
    | Variant { case_sensitivity; clauses } ->
      let clauses = List.map ~f:without_tag_list clauses in
      List.map clauses ~f:(fun ({ name; clause_kind } : Sexp_grammar.clause) ->
        let gen =
          let%bind name = Generator.of_list (name_examples ~case_sensitivity name) in
          match clause_kind with
          | Atom_clause -> return (Sexp.Atom name)
          | List_clause { args } ->
            let%map sexps = on_list_grammar args in
            Sexp.List (Sexp.Atom name :: sexps)
        in
        let min_recursive_calls =
          match clause_kind with
          | Atom_clause -> 0
          | List_clause { args } -> Min_recursive_calls.on_list_grammar args
        in
        gen, min_recursive_calls)
      |> recursive_union
    | Tycon _ ->
      let lazy_grammar = lazy (Sexp_grammar.unroll_tycon_untyped ~tag_prefix grammar) in
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
