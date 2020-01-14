open Types

let run_file fn state =
  try
  Eval.eval_command_list (read_file (Parser.file Lexer.token) fn) state
  with
  | InternalError err -> print_error err; (EvtBool false, state)
  | Sys.Break -> prerr_endline "Interrupted."; (EvtBool false, state)
  | e -> print_error (Nowhere, Fatal (Printexc.to_string e), state.stack); (EvtBool false, state)
