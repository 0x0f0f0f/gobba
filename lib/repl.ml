open Types
open Errors


let read_toplevel () =
  let prompt = "> " in
  let str = Ocamline.read
      ~prompt:prompt
      ~brackets:[('(', ')'); ('[',']');  ('{','}')]
      ~strings:['"']
      ";;" in
  Parsedriver.read_one str

let run_one = Eval.eval_command

let rec repl_loop state maxdepth internalst =
  while true do
    try
      let cmd = List.hd (read_toplevel ()) in
      state := snd (Eval.eval_command cmd !state (Filename.current_dir_name))
    with
    | End_of_file -> raise End_of_file
    | InternalError err ->
      if internalst then Printexc.print_backtrace stderr;
      print_error err;
      print_stacktrace err maxdepth;
      repl_loop state maxdepth internalst
    | Sys.Break ->
      if internalst then Printexc.print_backtrace stderr;
      prerr_endline "Interrupted.";
      repl_loop state maxdepth internalst
    | e ->
      if internalst then Printexc.print_backtrace stderr;
      print_error (Nowhere, (Fatal (Printexc.to_string e)), !state.stack);
      repl_loop state maxdepth internalst
  done


let repl state maxstackdepth internalst =
  Sys.catch_break true;
  let mstate = ref state in
  try
    let _ = repl_loop mstate maxstackdepth internalst in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()


let run_file fn state maxstackdepth internalst =
  try
    Eval.eval_command_list (Parsedriver.read_file  fn) state (Filename.dirname fn)
  with
  | InternalError err ->
    if internalst then Printexc.print_backtrace stderr;
    print_error err;
    print_stacktrace err maxstackdepth;
    (EvtBool false, state)
  | Sys.Break ->
    if internalst then Printexc.print_backtrace stderr;
    prerr_endline "Interrupted.";
    (EvtBool false, state)
  | e ->
    if internalst then Printexc.print_backtrace stderr;
    print_error (Nowhere, Fatal (Printexc.to_string e), state.stack);
    (EvtBool false, state)
