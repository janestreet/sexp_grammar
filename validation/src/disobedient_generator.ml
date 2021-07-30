open! Core
open Base_quickcheck
open Or_error.Let_syntax

module Edit = struct
  type t =
    | Lift of Sexp_index.t
    | Nest of Sexp_index.t
    | Remove of Sexp_index.t
    | Reverse of Sexp_index.t
    | Set of Sexp_index.t * Sexp_index.t
  [@@deriving compare, sexp_of, variants]

  let apply t prev =
    match t with
    | Lift i ->
      let%bind j = Sexp_index.parent i
      and next = Sexp_index.get i prev in
      Sexp_index.set j prev ~to_:next
    | Nest i ->
      let%bind sexp = Sexp_index.get i prev in
      Sexp_index.set i prev ~to_:(List [ sexp ])
    | Remove i -> Sexp_index.remove i prev
    | Reverse i ->
      (match%bind Sexp_index.get i prev with
       | Atom s -> Sexp_index.set i prev ~to_:(Atom (String.rev s))
       | List sexps -> Sexp_index.set i prev ~to_:(List (List.rev sexps)))
    | Set (i, j) ->
      let%bind next = Sexp_index.get j prev in
      Sexp_index.set i prev ~to_:next
  ;;

  let generate_edited_sexps sexp : Sexp.t Generator.t =
    match Sexp_index.enumerate sexp with
    | [] ->
      raise_s [%message "Impossible: every sexp has at least one index." (sexp : Sexp.t)]
    | [ i ] ->
      let%map.Generator t = Generator.of_list [ Nest i; Reverse i ] in
      apply t sexp |> ok_exn
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
      Variants.fold ~init:[] ~lift:f ~nest:f ~remove:f ~reverse:f ~set
      |> Generator.union
      |> Generator.filter_map ~f:(fun t -> apply t sexp |> Result.ok)
  ;;
end

let create_unfiltered grammar =
  Generator.bind (Obedient_generator.create grammar) ~f:Edit.generate_edited_sexps
;;

let create grammar =
  let validate = Staged.unstage (Validate_sexp.validate grammar) in
  Generator.filter_map (create_unfiltered grammar) ~f:(fun sexp ->
    match validate sexp with
    | Ok () -> None
    | Error error -> Some (sexp, error))
;;

module Private = struct
  let create_unfiltered = create_unfiltered
end
