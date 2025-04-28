open! Core
open! Import
open Sexp_grammar_completion_protocol

(* Many functions in this module return [_ Or_error.t] even when their callers will equate
   [Error _] with [Ok []]. The errors make the code easier to read and debug. We're not
   concerned about the performance impact of constructing the errors. *)

let exactly candidates : Candidates.t = { exhaustive = true; candidates }
let unknown : Candidates.t = { exhaustive = false; candidates = [] }

let concat (ts : Candidates.t list) : Candidates.t =
  { exhaustive = List.for_all ts ~f:(fun t -> t.exhaustive)
  ; candidates = List.concat_map ts ~f:(fun t -> t.candidates)
  }
;;

(* Dedent doesn't trim blank lines because it needs to let users define string constants
   with leading or trailing empty lines. E.g., imagine constructing a big message
   piecemeal and leaving blank lines as a form of quoting. *)
let trim_blank_lines l =
  List.drop_while l ~f:(String.for_all ~f:Char.is_whitespace)
  |> List.rev
  |> List.drop_while ~f:(String.for_all ~f:Char.is_whitespace)
  |> List.rev
;;

let get_doc (key, value) =
  match String.equal key Sexplib0.Sexp_grammar.doc_comment_tag with
  | false -> None
  | true ->
    (match [%of_sexp: string] value with
     | string -> Some (String.concat ~sep:"\n" (trim_blank_lines (Dedent.lines string)))
     | exception Of_sexp_error (_, _) -> None)
;;

let rec collect_tags with_tag_list =
  match (with_tag_list : _ Sexp_grammar.with_tag_list) with
  | No_tag grammar -> grammar, []
  | Tag { key; value; grammar } ->
    let grammar, tags = collect_tags grammar in
    grammar, (key, value) :: tags
;;

let should_suggest tags =
  match
    Sexp_grammar.first_tag_value tags Sexp_grammar.completion_suggested [%of_sexp: bool]
  with
  | None | Some (Error _) | Some (Ok true) -> true
  | Some (Ok false) -> false
;;

let clause_with_tag_list tags (clause : Sexp_grammar.clause) =
  List.fold tags ~init:(Sexp_grammar.No_tag clause) ~f:(fun grammar (key, value) ->
    Tag { key; value; grammar })
;;

let get_docs tags = List.filter_map tags ~f:get_doc

let rec enter_list_in_grammar grammar ~use_old_option_format =
  let open Sexp_grammar in
  match unroll_tycon_untyped grammar with
  | Tyvar _ | Tycon _ | Recursive _ ->
    raise_s [%message "internal error: recursive grammar must be unrolled" [%here]]
  | Any name -> Ok [ Many (Any name) ]
  | Bool | Char | Integer | Float | String ->
    error_s [%message "cannot enter list; grammar expected atom"]
  | Tagged { key = _; value = _; grammar } ->
    (* The only tags we care about are doc comments, and they cannot appear on arbitrary
       bits of list grammar. *)
    enter_list_in_grammar grammar ~use_old_option_format
  | Option grammar ->
    (match use_old_option_format with
     | true -> Ok [ Cons (grammar, Empty); Empty ]
     | false ->
       Ok
         [ Cons
             ( Variant
                 { case_sensitivity = Case_sensitive_except_first_character
                 ; clauses = [ No_tag { name = "Some"; clause_kind = Atom_clause } ]
                 }
             , Cons (grammar, Empty) )
         ])
  | Union grammars ->
    grammars
    |> List.map ~f:(enter_list_in_grammar ~use_old_option_format)
    |> Or_error.filter_ok_at_least_one
    |> Or_error.map ~f:List.concat
  | List list_grammar -> Ok [ list_grammar ]
  | Variant { case_sensitivity; clauses } ->
    let clause_as_list ({ name; clause_kind }, tags) =
      match clause_kind with
      | Atom_clause -> None
      | List_clause { args } ->
        Some
          (Cons
             ( Variant
                 { case_sensitivity
                 ; clauses =
                     [ clause_with_tag_list tags { name; clause_kind = Atom_clause } ]
                 }
             , args ))
    in
    (match List.filter_map clauses ~f:(clause_as_list << collect_tags) with
     | [] ->
       Or_error.error_s [%message "cannot enter variant type; grammar expects only atoms"]
     | _ :: _ as list_grammars -> Ok list_grammars)
  | Lazy lazy_grammar ->
    enter_list_in_grammar (Portable_lazy.force lazy_grammar) ~use_old_option_format
;;

let rec of_hole_in_grammar grammar ~use_old_option_format =
  let open Sexp_grammar in
  match unroll_tycon_untyped grammar with
  | Tyvar _ | Tycon _ | Recursive _ ->
    raise_s [%message "internal error: recursive grammar must be unrolled" [%here]]
  | Any _ | Char | Integer | Float | String -> unknown
  | Bool ->
    exactly
      (List.map Bool.all ~f:(fun bool : Candidate.t ->
         Add_atom
           { atom_signified = Bool.to_string bool
           ; case_sensitivity = Case_sensitive_except_first_character
           ; documentation = []
           }))
  | Option _ ->
    (match use_old_option_format with
     | true -> exactly [ Enter_list ]
     | false ->
       exactly
         [ Add_atom
             { atom_signified = "None"
             ; case_sensitivity = Case_sensitive_except_first_character
             ; documentation = []
             }
         ; Enter_list_and_add_atom
             { atom_signified = "Some"
             ; case_sensitivity = Case_sensitive_except_first_character
             ; documentation = []
             }
         ])
  | Union grammars ->
    grammars |> List.map ~f:(of_hole_in_grammar ~use_old_option_format) |> concat
  | List _ -> exactly [ Enter_list ]
  | Variant { case_sensitivity; clauses } ->
    let get_atom clause =
      let { name; clause_kind }, tags = collect_tags clause in
      match should_suggest tags with
      | false -> Second ()
      | true ->
        let documentation = get_docs tags in
        First
          (match clause_kind with
           | Atom_clause ->
             Candidate.Add_atom { atom_signified = name; documentation; case_sensitivity }
           | List_clause { args = _ } ->
             Candidate.Enter_list_and_add_atom
               { atom_signified = name; case_sensitivity; documentation })
    in
    let candidates, not_suggested = List.partition_map clauses ~f:get_atom in
    { candidates
    ; exhaustive =
        (match not_suggested with
         | [] -> true
         | _ :: _ -> false)
    }
  | Lazy lazy_grammar ->
    of_hole_in_grammar (Portable_lazy.force lazy_grammar) ~use_old_option_format
  | Tagged { key = _; value = _; grammar } ->
    of_hole_in_grammar grammar ~use_old_option_format
;;

let unknown_field here name fields =
  error_s
    [%message
      "Unknown field."
        name
        ~_:(here : Source_code_position.t)
        ~fields:(Map.keys fields : string list)]
;;

let consume_field fields sexp ~allow_extra_fields =
  match (sexp : Sexp.t) with
  | List (Atom name :: sexps) ->
    (match Map.find fields name with
     | None ->
       (match allow_extra_fields with
        | true -> Ok fields
        | false -> unknown_field [%here] name fields)
     | Some (({ name = _; required = _; args } : Sexp_grammar.field), _) ->
       let%bind.Or_error () =
         Staged.unstage (Sexp_grammar.validate_sexp_list args) sexps
       in
       Ok (Map.remove fields name))
  | _ -> error_s [%message "Expected a list starting with a field name." (sexp : Sexp.t)]
;;

let rec of_prefix_in_grammar grammar prefix ~use_old_option_format =
  match (prefix : Prefix.t) with
  | Hole -> Ok (of_hole_in_grammar grammar ~use_old_option_format)
  | In_list (sexps, prefix) ->
    let%bind.Or_error list_grammars =
      enter_list_in_grammar grammar ~use_old_option_format
    in
    List.filter_map list_grammars ~f:(fun list_grammar ->
      of_list_grammar list_grammar sexps prefix ~use_old_option_format |> Result.ok)
    |> concat
    |> Or_error.return

and of_list_grammar list_grammar sexps prefix ~use_old_option_format =
  match (list_grammar : Sexp_grammar.list_grammar) with
  | Fields { allow_extra_fields; fields } ->
    let%bind.Or_error fields =
      match
        fields
        |> List.map ~f:(fun field ->
          let field, tags = collect_tags field in
          field.name, (field, tags))
        |> Map.of_alist (module String)
      with
      | `Ok fields -> Ok fields
      | `Duplicate_key field_name ->
        Or_error.error_s [%message "duplicate field name in record grammar" ~field_name]
    in
    let%bind.Or_error fields_not_seen_yet =
      List.fold sexps ~init:(Ok fields) ~f:(fun fields sexp ->
        let%bind.Or_error fields in
        consume_field fields sexp ~allow_extra_fields)
    in
    of_fields fields_not_seen_yet prefix ~allow_extra_fields ~use_old_option_format
  | Empty ->
    (match sexps, prefix with
     | [], Hole -> Ok (exactly [])
     | [], In_list (_, _) | _ :: _, (Hole | In_list (_, _)) ->
       error_s [%message "Expected end of list."])
  | Cons (first, rest) ->
    (match sexps with
     | [] -> of_prefix_in_grammar first prefix ~use_old_option_format
     | sexp :: sexps ->
       let%bind.Or_error () =
         Staged.unstage (Sexp_grammar.validate_sexp_untyped first) sexp
       in
       of_list_grammar rest sexps prefix ~use_old_option_format)
  | Many grammar -> of_many grammar sexps prefix ~use_old_option_format

and of_fields fields prefix ~allow_extra_fields ~use_old_option_format =
  let exhaustive =
    (not allow_extra_fields)
    && Map.exists fields ~f:(fun ({ name = _; required; args = _ }, _) -> required)
  in
  match prefix with
  | In_list ([], Hole) ->
    let clauses =
      List.map (Map.to_alist fields) ~f:(fun (name, ((_ : Sexp_grammar.field), tags)) ->
        clause_with_tag_list tags { name; clause_kind = Atom_clause })
    in
    let candidates =
      of_hole_in_grammar
        (Variant { case_sensitivity = Case_sensitive; clauses })
        ~use_old_option_format
    in
    Ok { candidates with exhaustive }
  | In_list (Atom name :: rest, prefix) ->
    (match Map.find fields name with
     | Some ({ name = _; required = _; args }, _comments) ->
       of_list_grammar args rest prefix ~use_old_option_format
     | None ->
       (match allow_extra_fields with
        | true -> Ok (exactly [])
        | false -> unknown_field [%here] name fields))
  | Hole ->
    let remaining_fields : Sexp_grammar.grammar =
      let clauses =
        List.map (Map.data fields) ~f:(fun ({ name; required = _; args }, tags) ->
          { name; clause_kind = List_clause { args } } |> clause_with_tag_list tags)
      in
      Variant { case_sensitivity = Case_sensitive; clauses }
    in
    let candidates = of_hole_in_grammar remaining_fields ~use_old_option_format in
    Ok { candidates with exhaustive }
  | In_list (List _ :: _, _) | In_list ([], In_list _) ->
    raise_s [%message "Expected record fields" (prefix : Prefix.t)]

and of_many grammar sexps prefix ~use_old_option_format =
  match sexps with
  | [] ->
    let%bind.Or_error candidates =
      of_prefix_in_grammar grammar prefix ~use_old_option_format
    in
    (* [exhaustive = false] because we don't suggest ending the list. *)
    Ok { candidates with exhaustive = false }
  | sexp :: sexps ->
    let%bind.Or_error () =
      Staged.unstage (Sexp_grammar.validate_sexp_untyped grammar) sexp
    in
    of_many grammar sexps prefix ~use_old_option_format
;;

let complete (grammar : _ Sexp_grammar.t) =
  (* Extract this state once, outside the staged function, so we can use a consistent
     format for completion. *)
  let use_old_option_format = Dynamic.get Sexplib0.Sexp_conv.write_old_option_format in
  Staged.stage (fun prefix ->
    let%map.Or_error completion =
      of_prefix_in_grammar grammar.untyped prefix ~use_old_option_format
    in
    { completion with
      candidates = completion.candidates |> List.dedup_and_sort ~compare:Candidate.compare
    })
;;

let command_auto_complete (grammar : _ Sexp_grammar.t) =
  let get_candidates = complete grammar in
  Staged.stage (fun _ ~part ->
    match
      let%map.Option prefix, atom =
        Prefix.of_substring part ~pos:0 ~len:(String.length part)
      in
      let%map.Or_error candidates = Staged.unstage get_candidates prefix in
      (* Also note that we do the prefix filtering here instead of client-side *)
      List.filter candidates.candidates ~f:(fun c -> Candidate.matches_atom_prefix c atom)
      |> List.map ~f:Candidate.insert_to_left
    with
    | Some (Ok l) -> l
    | Some (Error _) | None -> [])
;;
