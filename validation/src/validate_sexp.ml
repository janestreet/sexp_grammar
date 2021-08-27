open! Base

module Validation_callbacks = struct
  open Or_error.Let_syntax

  type t = (Sexp.t -> unit Or_error.t) Staged.t

  let any (_ : string) = Staged.stage (fun (_ : Sexp.t) -> Ok ())

  module type Of_sexpable = sig
    type t [@@deriving of_sexp]
  end

  let of_type name (module M : Of_sexpable) =
    Staged.stage (fun sexp ->
      match M.t_of_sexp sexp with
      | (_ : M.t) -> Ok ()
      | exception reason ->
        let s = Printf.sprintf "invalid %s" name in
        Or_error.error_s [%message s (reason : exn)])
  ;;

  module Z = struct
    type t = Zarith.Z.t

    (* Equivalent to [Bigint.t_of_sexp], without depending on all of [Core]. *)
    include Sexpable.Of_stringable (Zarith.Z)
  end

  let bool = of_type "bool" (module Bool)
  let char = of_type "char" (module Char)
  let integer = of_type "integer" (module Z)
  let float = of_type "float" (module Float)
  let string = of_type "string" (module String)
  let tag f _ _ = f

  let option f =
    let read_old_option_format = !Sexplib0.Sexp_conv.read_old_option_format in
    Staged.stage (fun sexp ->
      match (sexp : Sexp.t) with
      | Atom ("none" | "None") -> Ok ()
      | List [ Atom ("some" | "Some"); value_sexp ] -> Staged.unstage f value_sexp
      | List [] when read_old_option_format -> Ok ()
      | List [ value_sexp ] when read_old_option_format -> Staged.unstage f value_sexp
      | _ -> Or_error.error_s [%message "expected an option" (sexp : Sexp.t)])
  ;;

  let union fs =
    Staged.stage (fun sexp ->
      match Or_error.find_ok (List.map fs ~f:(fun f -> Staged.unstage f sexp)) with
      | Ok _ as ok -> ok
      | Error error ->
        let s = "expected union of several grammars, but none were satisfied." in
        Or_error.error_s [%message s ~_:(error : Error.t)])
  ;;

  let lazy_ lazy_f = Staged.stage (fun sexp -> Staged.unstage (Lazy.force lazy_f) sexp)
  let of_lazy_recursive = lazy_

  type list_t = (Sexp.t list -> unit Or_error.t) Staged.t

  let list list_t =
    Staged.stage (fun sexp ->
      match (sexp : Sexp.t) with
      | List sexps -> Staged.unstage list_t sexps
      | Atom _ -> Or_error.error_s [%message "expected a list" (sexp : Sexp.t)])
  ;;

  let empty =
    Staged.stage (fun remaining_sexps ->
      if List.is_empty remaining_sexps
      then Ok ()
      else
        Or_error.error_s
          [%message "too many sexps in list" (remaining_sexps : Sexp.t list)])
  ;;

  let cons t list_t =
    Staged.stage (fun sexps ->
      match sexps with
      | [] -> Or_error.error_s [%message "too few sexps in list"]
      | head :: tail ->
        let%bind () = Staged.unstage t head
        and () = Staged.unstage list_t tail in
        Ok ())
  ;;

  let many t =
    Staged.stage (fun sexps ->
      match List.map sexps ~f:(Staged.unstage t) |> Or_error.all_unit with
      | Ok _ as ok -> ok
      | Error error ->
        let s = "Some items in list did not satisfy grammar." in
        Or_error.error_s [%message s ~_:(error : Error.t)])
  ;;

  module Seen_or_unseen = struct
    type 'a t =
      | Unseen of 'a
      | Seen
  end

  let require_list_with_leading_atom name sexp =
    match (sexp : Sexp.t) with
    | List (Atom field_name :: sexps) -> Ok (field_name, sexps)
    | Atom _ | List [] | List (List _ :: _) ->
      let s = Printf.sprintf "invalid %s; expected a list with a leading atom" name in
      Or_error.error_s [%message s (sexp : Sexp.t)]
  ;;

  let field fields ~allow_extra_fields =
    let stop_error sexp = Continue_or_stop.Stop (Or_error.error_s sexp) in
    Staged.stage (fun sexp : (_, _) Continue_or_stop.t ->
      match require_list_with_leading_atom "record field" sexp with
      | Error _ as stop -> Stop stop
      | Ok (field_name, sexps) ->
        (match (Map.find fields field_name : _ Seen_or_unseen.t option) with
         | None ->
           if allow_extra_fields
           then Continue fields
           else
             stop_error
               [%message
                 "unrecognized record field"
                   (field_name : string)
                   ~recognized:(Map.keys fields : string list)
                   (sexp : Sexp.t)]
         | Some Seen ->
           stop_error [%message "duplicate record field" (field_name : string)]
         | Some (Unseen (Required t_list | Optional t_list : _ Sexp_grammar.Field.t)) ->
           (match Staged.unstage t_list sexps with
            | Ok () -> Continue (Map.set fields ~key:field_name ~data:Seen)
            | Error _ as reject -> Stop reject)))
  ;;

  let record fields ~allow_extra_fields =
    let fields =
      match
        fields
        |> List.Assoc.map ~f:(fun (field, _) -> Seen_or_unseen.Unseen field)
        |> Map.of_alist (module String)
      with
      | `Ok fields -> fields
      | `Duplicate_key field_name ->
        raise_s [%message "duplicate field name in grammar" ~field_name]
    in
    Staged.stage (fun sexps ->
      List.fold_until
        sexps
        ~init:fields
        ~f:(fun fields sexp -> Staged.unstage (field fields ~allow_extra_fields) sexp)
        ~finish:(fun fields ->
          Or_error.find_map_ok (Map.to_alist fields) ~f:(fun (field_name, status) ->
            match status with
            | Seen | Unseen (Optional _) -> Ok ()
            | Unseen (Required _) ->
              Or_error.error_s [%message "missing record field" (field_name : string)])))
  ;;

  let variant clauses ~case_sensitivity =
    let (module Name) =
      Sexp_grammar.Case_sensitivity.to_string_comparator case_sensitivity
    in
    let clauses =
      match Map.of_alist (module Name) clauses with
      | `Ok clauses -> clauses
      | `Duplicate_key clause_name ->
        raise_s [%message "duplicate clause name in grammar" ~clause_name]
    in
    Staged.stage (fun sexp ->
      let%bind clause_name, maybe_sexps =
        match (sexp : Sexp.t) with
        | Atom name -> Ok (name, None)
        | List (Atom name :: args) -> Ok (name, Some args)
        | List [] | List (List _ :: _) ->
          Or_error.error_s
            [%message
              "invalid variant; expected an atom or a list with a leading atom"
                (sexp : Sexp.t)]
      in
      match Map.find clauses clause_name with
      | None ->
        Or_error.error_s
          [%message
            "invalid variant; unrecognized name"
              (clause_name : string)
              (case_sensitivity : Sexp_grammar.Case_sensitivity.t)
              ~recognized:(Map.keys clauses : string list)]
      | Some (maybe_t_list, _) ->
        (match maybe_sexps, maybe_t_list with
         | None, None -> Ok ()
         | None, Some _ ->
           let s =
             Printf.sprintf
               "invalid %S variant; expected a list but got an atom"
               clause_name
           in
           Or_error.error_s [%message s (sexp : Sexp.t)]
         | Some _, None ->
           let s =
             Printf.sprintf
               "invalid %S variant; expected an atom but got a list"
               clause_name
           in
           Or_error.error_s [%message s (sexp : Sexp.t)]
         | Some sexps, Some t_list -> Staged.unstage t_list sexps))
  ;;
end

module Validation = Sexp_grammar.Fold_recursive (Validation_callbacks)

let validate_sexp = Validation.of_typed_grammar_exn
let validate_sexp_untyped = Validation.of_grammar_exn
let validate_sexp_list = Validation.of_list_grammar_exn
