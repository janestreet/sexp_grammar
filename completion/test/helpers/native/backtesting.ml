open! Core
open Async

let is_bad check file =
  match%bind
    Deferred.Or_error.try_with ~run:`Now ~rest:`Raise (fun () ->
      let%map s = Reader.file_contents file in
      with_return (fun { return } ->
        check s ~on_error:(fun _ _ ~pos:_ -> return true);
        return false))
  with
  | Ok bad -> return bad
  | Error _ -> return true
;;

let batch_command grammar =
  Command.async
    ~summary:"backtest against multiple files"
    (let%map_open.Command () = return ()
     and files = anon (sequence ("FILE" %: Filename_unix.arg_type)) in
     fun () ->
       let check =
         Staged.unstage
           (Sexp_grammar_completion_test_helpers.Spot_check.batchwise_check_every_position
              grammar)
       in
       Deferred.List.filter files ~how:`Sequential ~f:(is_bad check)
       >>| List.iter ~f:print_endline)
    ~behave_nicely_in_pipeline:false
;;

let single_command grammar =
  Command.async
    ~summary:"backtest against one file, with helpful errors"
    (let%map_open.Command () = return ()
     and file = anon ("FILE" %: Filename_unix.arg_type)
     and verbose =
       flag
         "-verbose"
         no_arg
         ~doc:
           " show positions with no suggestions or where inexhaustive suggestions were \
            inexhaustive"
     in
     fun () ->
       let%bind s = Reader.file_contents file in
       (match verbose with
        | true -> Sexp_grammar_completion_test_helpers.check_every_position grammar s
        | false ->
          let check =
            Staged.unstage
              (Sexp_grammar_completion_test_helpers.Spot_check
               .batchwise_check_every_position
                 grammar)
          in
          check s ~on_error:(fun spot_check s ~pos ->
            print_endline
              (Sexp_grammar_completion_test_helpers.mark_positions s ~pos:[ pos ]);
            print_s
              [%sexp (spot_check : Sexp_grammar_completion_test_helpers.Spot_check.t)]));
       return ())
    ~behave_nicely_in_pipeline:false
;;

let command grammar ~name =
  Command.group
    ~summary:[%string "commands for backtesting %{name} sexp grammar against real input"]
    [ "batch", batch_command grammar; "single", single_command grammar ]
;;
