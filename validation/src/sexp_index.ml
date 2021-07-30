open! Core
open Or_error.Let_syntax

type t = int list [@@deriving compare, equal, sexp_of]

let traverse t sexp ~on_dst ~on_list =
  let invalid =
    lazy (error_s [%message "Invalid index." ~_:(t : t) ~_:(sexp : Sexp.t)])
  in
  let rec zoom_out x ~context =
    match context with
    | [] -> Ok x
    | (before, after) :: context -> on_list ~before ~after x >>= zoom_out ~context
  in
  let rec zoom_in t (sexp : Sexp.t) ~context =
    match t with
    | [] -> on_dst sexp >>= zoom_out ~context
    | head :: tail ->
      (match sexp with
       | Atom _ -> force invalid
       | List sexps ->
         (match List.split_n sexps head with
          | _, [] -> force invalid
          | before, at :: after -> zoom_in tail at ~context:((before, after) :: context)))
  in
  zoom_in t sexp ~context:[]
;;

let get =
  traverse ~on_dst:Or_error.return ~on_list:(fun ~before:_ ~after:_ sexp -> Ok sexp)
;;

let remove t sexp =
  let on_dst _ = Ok None in
  let on_list ~before ~after sexp =
    Ok (Some (Sexp.List (List.concat [ before; Option.to_list sexp; after ])))
  in
  match%bind traverse t sexp ~on_dst ~on_list with
  | Some sexp -> Ok sexp
  | None -> error_s [%message "Cannot remove entire sexp."]
;;

let update t sexp ~f =
  traverse t sexp ~on_dst:f ~on_list:(fun ~before ~after sexp ->
    Ok (Sexp.List (List.concat [ before; [ sexp ]; after ])))
;;

let set t sexp ~to_ = update t sexp ~f:(fun _ -> Ok to_)

let rec enumerate sexp : t list =
  let non_empty =
    match (sexp : Sexp.t) with
    | Atom _ -> []
    | List sexps ->
      List.concat_mapi sexps ~f:(fun i sexp ->
        List.map (enumerate sexp) ~f:(fun t -> i :: t))
  in
  [] :: non_empty
;;

let parent t =
  match List.drop_last t with
  | None -> error_s [%message "No parent." ~_:(t : t)]
  | Some t -> Ok t
;;
