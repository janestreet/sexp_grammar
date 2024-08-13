open! Core
open! Async
open! Import

let create grammar ~summary =
  let complete = lazy (unstage (Sexp_grammar_completion.complete grammar)) in
  let implementations =
    Sexp_grammar_completion_protocol.Rpc_complete.implement_multi
      (fun (_ : Command_rpc.Command.Invocation.t) ~version:_ query ->
         return (ok_exn ((force complete) query)))
  in
  Command_rpc.Command.create ~summary [ `Implementations implementations ]
;;
