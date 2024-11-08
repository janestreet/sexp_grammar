open! Core
open! Async_kernel
open! Import

let default_mark = "│"

let mark_positions ?(mark = default_mark) s ~pos =
  List.dedup_and_sort (String.length s :: pos) ~compare:[%compare: int]
  |> List.folding_map ~init:0 ~f:(fun prev next ->
    next, String.sub s ~pos:prev ~len:(next - prev))
  |> String.concat ~sep:mark
;;

module Spot_check = struct
  type t =
    | Failed_to_compute_suggestions of Error.t
    | No_suggestions_here
    | Inexhaustive_suggestions_did_not_match_input
    | Exhaustive_suggestions_did_not_match_input of Candidates.t
    | Suggestions_consistent_with_input
  [@@deriving sexp_of]

  let print_cr_showing_pos t s ~pos =
    print_endline (mark_positions s ~pos);
    print_cr [%sexp (t : t)]
  ;;

  let create complete s ~pos =
    match Prefix.of_substring s ~pos:0 ~len:pos with
    | None -> Suggestions_consistent_with_input
    | Some (in_sexp, atom_prefix) ->
      (match complete in_sexp with
       | Error error -> Failed_to_compute_suggestions error
       | Ok completion ->
         (match Candidates.candidates completion with
          | [] -> No_suggestions_here
          | _ :: _ as suggestions ->
            (match Candidate_helpers.matches suggestions atom_prefix s ~pos with
             | true -> Suggestions_consistent_with_input
             | false ->
               (match Candidates.exhaustive completion with
                | true -> Exhaustive_suggestions_did_not_match_input completion
                | false -> Inexhaustive_suggestions_did_not_match_input))))
  ;;

  let check_every_position grammar s =
    let no_suggestions_at_pos = Queue.create () in
    let inexhaustive_suggestions_did_not_match_input_at_pos = Queue.create () in
    let complete = Staged.unstage (Sexp_grammar_completion.complete grammar) in
    String.iteri s ~f:(fun pos _ ->
      match create complete s ~pos with
      | Failed_to_compute_suggestions _ as t -> print_cr_showing_pos t s ~pos:[ pos ]
      | No_suggestions_here -> Queue.enqueue no_suggestions_at_pos pos
      | Inexhaustive_suggestions_did_not_match_input ->
        Queue.enqueue inexhaustive_suggestions_did_not_match_input_at_pos pos
      | Exhaustive_suggestions_did_not_match_input _ as t ->
        print_cr_showing_pos t s ~pos:[ pos ]
      | Suggestions_consistent_with_input -> ());
    let show_queue pos message =
      match Queue.to_list pos with
      | [] -> ()
      | _ :: _ as pos ->
        print_endline "";
        print_endline message;
        print_endline (String.make (String.length message) '-');
        print_endline (mark_positions s ~pos)
    in
    show_queue
      inexhaustive_suggestions_did_not_match_input_at_pos
      "Inexhaustive suggestions contained no matches at the following locations:";
    show_queue no_suggestions_at_pos "No suggestions at the following locations:"
  ;;

  let batchwise_check_every_position grammar =
    let complete = Staged.unstage (Sexp_grammar_completion.complete grammar) in
    stage (fun s ~on_error ->
      for pos = 0 to String.length s do
        match create complete s ~pos with
        | (Failed_to_compute_suggestions _ | Exhaustive_suggestions_did_not_match_input _)
          as t -> on_error t s ~pos
        | No_suggestions_here
        | Inexhaustive_suggestions_did_not_match_input
        | Suggestions_consistent_with_input -> ()
      done)
  ;;

  let quickcheck ?cr grammar to_string =
    let check = Staged.unstage (batchwise_check_every_position grammar) in
    let obedient_generator =
      Sexp_grammar_validation.Private.Obedient_generator.create grammar
    in
    let sexp_matches_grammar =
      Staged.unstage (Sexp_grammar.validate_sexp grammar) >> Result.is_ok
    in
    quickcheck_m
      ?cr
      (module struct
        type t = Sexp.t [@@deriving sexp_of]

        let quickcheck_generator = obedient_generator

        let quickcheck_shrinker =
          Base_quickcheck.Shrinker.sexp
          |> Base_quickcheck.Shrinker.filter ~f:sexp_matches_grammar
        ;;
      end)
      ~f:(fun sexp ->
        let s = to_string sexp in
        check s ~on_error:(fun t s ~pos -> print_cr ?cr [%message s (pos : int) (t : t)]))
  ;;
end

let check_every_position = Spot_check.check_every_position
let quickcheck = Spot_check.quickcheck

let show_packaged ?(mark = default_mark) ?pos complete s =
  let pos = Option.value pos ~default:(String.length s) in
  let s = String.subo s ~len:pos in
  print_endline (s ^ mark);
  print_endline "=>";
  match Prefix.of_substring s ~pos:0 ~len:pos with
  | None ->
    print_s [%sexp "Cannot find prefix."];
    Packaged.return ()
  | Some (in_sexp, atom_prefix) ->
    (match%map.Packaged complete in_sexp with
     | Error error -> print_cr [%message (error : Error.t)]
     | Ok completion ->
       (match Candidates.exhaustive completion with
        | false -> print_endline "(inexhaustive)"
        | true -> ());
       let insertions =
         List.map (Candidates.candidates completion) ~f:(fun candidate ->
           let matches_atom_prefix =
             Candidate.matches_atom_prefix candidate atom_prefix
           in
           let insert =
             let left = Candidate.insert_to_left candidate in
             let right = Candidate.insert_to_right candidate in
             let len = pos - Candidate_helpers.backspace atom_prefix in
             let prefix = String.sub s ~pos:0 ~len in
             [%string "%{prefix}%{left}%{mark}%{right}"]
           in
           let documentation = Candidate.documentation candidate in
           matches_atom_prefix, insert, documentation)
       in
       let insertion_max_width =
         List.map insertions ~f:(snd3 >> String.length)
         |> List.max_elt ~compare:[%compare: int]
         |> Option.value ~default:0
       in
       List.iter insertions ~f:(fun (matches_atom_prefix, insert, documentation) ->
         (match matches_atom_prefix with
          | false -> print_endline insert
          | true -> printf "%-*s (matches atom prefix)\n" insertion_max_width insert);
         List.iter documentation ~f:(fun s ->
           String.split_lines s
           |> List.map ~f:(sprintf "> %s")
           |> String.concat ~sep:"\n"
           |> printf "-- doc comment:\n%s\n")))
;;

let show ?mark ?pos complete s =
  show_packaged ?mark ?pos (fun in_sexp -> complete in_sexp |> Packaged.Ident.pack) s
  |> Packaged.Ident.unpack
;;

let show_async ?mark ?pos complete s =
  show_packaged ?mark ?pos (fun in_sexp -> complete in_sexp |> Packaged.Deferred.pack) s
  |> Packaged.Deferred.unpack
;;

let show_sexp grammar s ~pos =
  match Prefix.of_substring s ~pos:0 ~len:pos with
  | None -> print_s [%sexp "Cannot find prefix."]
  | Some (in_sexp, _) ->
    let complete = Staged.unstage (Sexp_grammar_completion.complete grammar) in
    print_s [%sexp (complete in_sexp : Candidates.t Or_error.t)]
;;
