open! Core
open Base_quickcheck
open Or_error.Let_syntax

module Edit = struct
  type t =
    | Change_case of Sexp_index.t
    | Lift of Sexp_index.t
    | Nest of Sexp_index.t
    | Remove of Sexp_index.t
    | Reverse of Sexp_index.t
    | Set of Sexp_index.t * Sexp_index.t
  [@@deriving compare, sexp_of, variants]

  let case_changes_of original =
    let variations =
      let all_uppercase = String.uppercase original in
      let all_lowercase = String.lowercase original in
      let titlecase = String.capitalize all_lowercase in
      let swap_one_character =
        List.init (String.length original) ~f:(fun i ->
          String.mapi original ~f:(fun j c ->
            if i = j
            then if Char.is_uppercase c then Char.lowercase c else Char.uppercase c
            else c))
      in
      String.Set.of_list
        (all_uppercase :: all_lowercase :: titlecase :: swap_one_character)
    in
    Set.remove variations original
  ;;

  let generate_possible_edits t prev : Sexp.t Or_error.t list Or_error.t =
    match t with
    | Lift i ->
      let%map j = Sexp_index.parent i
      and next = Sexp_index.get i prev in
      [ Sexp_index.set j prev ~to_:next ]
    | Nest i ->
      let%map sexp = Sexp_index.get i prev in
      [ Sexp_index.set i prev ~to_:(List [ sexp ]) ]
    | Remove i -> Ok [ Sexp_index.remove i prev ]
    | Reverse i ->
      (match%map Sexp_index.get i prev with
       | Atom s -> [ Sexp_index.set i prev ~to_:(Atom (String.rev s)) ]
       | List sexps -> [ Sexp_index.set i prev ~to_:(List (List.rev sexps)) ])
    | Set (i, j) ->
      let%map next = Sexp_index.get j prev in
      [ Sexp_index.set i prev ~to_:next ]
    | Change_case i ->
      (match%map Sexp_index.get i prev with
       | List _ -> []
       | Atom s ->
         List.map
           (Set.to_list (case_changes_of s))
           ~f:(fun s -> Sexp_index.set i prev ~to_:(Atom s)))
  ;;

  let generate_edits t ~sexp =
    generate_possible_edits t sexp >>= Or_error.filter_ok_at_least_one |> Or_error.ok
  ;;

  let generate_edited_sexps sexp : Sexp.t Generator.t =
    match Sexp_index.enumerate sexp with
    | [] ->
      raise_s [%message "Impossible: every sexp has at least one index." (sexp : Sexp.t)]
    | [ i ] ->
      let take acc (v : _ Variant.t) = v.constructor i :: acc in
      let skip acc _ = acc in
      Variants.fold
        ~init:[]
        ~change_case:take
        ~lift:skip
        ~nest:take
        ~remove:skip
        ~reverse:take
        ~set:skip
      |> List.filter_map ~f:(generate_edits ~sexp)
      (* Choose an [Edit.t] uniformly, then choose among its concrete edits. *)
      |> List.map ~f:Generator.of_list
      |> Generator.union
    | indices ->
      let index_generator = Generator.of_list indices in
      let f acc (variant : _ Variant.t) =
        Generator.map index_generator ~f:variant.constructor :: acc
      in
      let set acc (_ : _ Variant.t) =
        let generator =
          Generator.filter_map
            (Generator.both index_generator index_generator)
            ~f:(fun (i, j) -> if Sexp_index.equal i j then None else Some (Set (i, j)))
        in
        generator :: acc
      in
      Variants.fold ~init:[] ~change_case:f ~lift:f ~nest:f ~remove:f ~reverse:f ~set
      |> Generator.union
      |> Generator.filter_map ~f:(generate_edits ~sexp)
      |> Generator.bind ~f:Generator.of_list
  ;;
end

let create_unfiltered grammar =
  Generator.bind (Obedient_generator.create grammar) ~f:Edit.generate_edited_sexps
;;

let create grammar =
  let validate = Staged.unstage (Sexp_grammar.validate_sexp grammar) in
  Generator.filter_map (create_unfiltered grammar) ~f:(fun sexp ->
    match validate sexp with
    | Ok () -> None
    | Error error -> Some (sexp, error))
;;

module Private = struct
  let create_unfiltered = create_unfiltered
end
