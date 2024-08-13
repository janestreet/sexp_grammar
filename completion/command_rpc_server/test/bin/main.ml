open! Core
open! Async

module Test_menu = struct
  type t = unit [@@deriving sexp_grammar]

  let command =
    Sexp_grammar_completion_command_rpc_server.create t_sexp_grammar ~summary:"_"
  ;;
end

let () = Command_unix.run (Command.group ~summary:"_" [ "test-menu", Test_menu.command ])
