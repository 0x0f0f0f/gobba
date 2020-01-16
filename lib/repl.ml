open Types
open Errors

let read_one parser str =
  parser (Lexing.from_string (str ^ "\n"))

let read_toplevel parser () =
  let prompt = "> " in
  let str = Ocamline.read
      ~prompt:prompt
      ~brackets:[('(', ')'); ('[',']');  ('{','}')]
      ~strings:['"']
      ";;" in
  parser (Lexing.from_string (str ^ "\n"))

let parser = Parser.toplevel Lexer.token

let rec read_lines_until ic del =
  let line = input_line ic in
  if (String.length line) < (String.length del) then
    line
  else if (String.sub (String.trim line)
             ((String.length line) - (String.length del))
             (String.length del)) = del
  then line
  else line ^ (read_lines_until ic del)

let run_one = Eval.eval_command

let rec repl_loop state maxdepth internalst =
  while true do
    try
      let cmd = read_toplevel parser () in
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
    Eval.eval_command_list (read_file (Parser.file Lexer.token) fn) state (Filename.dirname fn)
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
