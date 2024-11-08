open! Core
open! Async_kernel
open! Import
open Candidate
module Automaton = Parsexp.Private.Automaton

let backspace atom_prefix =
  match atom_prefix with
  | None -> 0
  | Some atom_prefix -> Atom_prefix.get_signifier_length atom_prefix
;;

module Observed = struct
  type t =
    | Atom of { signified : string }
    | Eof
    | Left_paren of { and_atom : string option }
    | Right_paren
  [@@deriving sexp_of]

  let saw_right_paren =
    let right_paren_message =
      lazy
        (match
           Parsexp.Parse_error.Private.raise
             Closed_paren_without_opened
             Parsexp.Positions.beginning_of_file
             ~at_eof:false
             ~atom_buffer:(Buffer.create 1)
         with
         | nothing -> Nothing.unreachable_code nothing
         | exception exn ->
           (match exn with
            | Parsexp.Parse_error error -> Parsexp.Parse_error.message error
            | _ -> raise_s [%message "Expected Parse_error" (exn : exn)]))
    in
    function
    | Parsexp.Parse_error error ->
      String.( = ) (Parsexp.Parse_error.message error) (force right_paren_message)
    | _ -> false
  ;;

  let create s ~pos:pos0 =
    (* [of_eof] handles two cases where the parser won't realize it has seen all the input
       until it receives the next character. The first such case is eof. The second such
       case is an unmatched right paren.

       I think there should be a way to feed the parser an explicit eof s.t. we can
       interpret the resulting state the same way as in the other big match in [feed], but
       I can't quite figure it out. *)
    let of_eof (sexp_prefix : Sexp_prefix.t option) ~default =
      match sexp_prefix with
      | None | Some ([], Hole None) -> default
      | Some ([], Hole (Some atom_prefix)) ->
        (match Atom_prefix.get_signified atom_prefix with
         | Complete { prefix } -> Atom { signified = prefix }
         | Incomplete _ ->
           raise_s [%message "Unexpected end" (atom_prefix : Atom_prefix.t) (default : t)])
      | Some (_, In_list _) ->
        (* If we're handling an eof, [In_list] is unexpected because we assume well-formed
           sexps.

           If we're handling an unexpected right-paren, [In_list] is unexpected because
           then the right-paren would not be unexpected. *)
        raise_s [%message "Unexpected end while in list?" (pos0 : int)]
      | Some (Atom signified :: _, _) -> Atom { signified }
      | Some (List sexps :: _, _) ->
        (match sexps with
         | Atom signified :: _ -> Left_paren { and_atom = Some signified }
         | List _ :: _ | [] -> Left_paren { and_atom = None })
    in
    let rec feed state stack s ~pos =
      match pos < String.length s with
      | false -> of_eof (Sexp_prefix.create state stack) ~default:Eof
      | true ->
        (match Automaton.feed state s.[pos] stack with
         | exception exn ->
           (match saw_right_paren exn with
            | false -> raise_s [%message "Unexpected" (exn : exn)]
            | true ->
              (* We must repeat the parse because [state] went into an error state upon
                 encountering the right paren. *)
              Sexp_prefix.of_substring s ~pos:pos0 ~len:(pos - pos0)
              |> of_eof ~default:Right_paren)
         | stack ->
           (match Sexp_prefix.create state stack with
            | None | Some ([], Hole _) | Some ([], In_list ([], Hole _)) ->
              (* Keep going until we finish an atom, see a right-paren, or see a nested
                 left-paren. *)
              feed state stack s ~pos:(pos + 1)
            | Some (Atom signified :: _, _) -> Atom { signified }
            | Some ([], In_list (List _ :: _, _))
            | Some ([], In_list ([], In_list (_, _)))
            | Some (List [] :: _, _)
            | Some (List (List _ :: _) :: _, _) -> Left_paren { and_atom = None }
            | Some ([], In_list (Atom signified :: _, _))
            | Some (List (Atom signified :: _) :: _, _) ->
              Left_paren { and_atom = Some signified }))
    in
    match pos0 = String.length s with
    | true -> Eof
    | false -> feed (Automaton.create Many Sexp_with_positions) Empty s ~pos:pos0
  ;;
end

let equal_atom ~case_sensitivity a b =
  Comparable.lift
    String.equal
    ~f:
      (match (case_sensitivity : Candidate.Case_sensitivity.t) with
       | Case_sensitive_except_first_character -> String.uncapitalize
       | Case_sensitive -> Fn.id
       | Case_insensitive -> String.uppercase)
    a
    b
;;

let matches_observed s ~pos =
  let observed = Observed.create s ~pos in
  match observed with
  | Atom { signified = observed } ->
    (function
      | Add_atom { atom_signified; case_sensitivity; documentation = _ } ->
        equal_atom ~case_sensitivity atom_signified observed
      | Enter_list | Enter_list_and_add_atom _ -> false)
  | Eof ->
    (* This typically means the file chose to end rather than start a new thing. *)
    const true
  | Left_paren { and_atom = None } ->
    (function
      | Enter_list | Enter_list_and_add_atom _ -> true
      | Add_atom _ -> false)
  | Left_paren { and_atom = Some observed } ->
    (function
      | Enter_list -> true
      | Enter_list_and_add_atom { atom_signified; case_sensitivity; documentation = _ } ->
        equal_atom ~case_sensitivity atom_signified observed
      | Add_atom _ -> false)
  | Right_paren ->
    (* We suggest ending lists by returning a non-exhaustive completion with no
       suggestions. *)
    const false
;;

let matches candidates atom_prefix s ~pos =
  List.exists candidates ~f:(matches_observed s ~pos:(pos - backspace atom_prefix))
;;
