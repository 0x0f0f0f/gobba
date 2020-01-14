open Types

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

let rec repl_loop state  =
  let loop () =
    let cmd = read_toplevel parser () in
    let _, newstate = Eval.eval_command cmd state in
    let _ = repl_loop newstate in ()
  in
  try
    loop ()
  with
  | End_of_file -> raise End_of_file
  | InternalError err ->
    Printexc.print_backtrace stderr;
    print_error err;
    print_stacktrace err 20;
    repl_loop state
  | Sys.Break -> prerr_endline "Interrupted."; repl_loop state
  | e ->
    Printexc.print_backtrace stderr;
    print_error (Nowhere, (Fatal (Printexc.to_string e)), state.stack);
    repl_loop state

let repl state =
  Sys.catch_break true;
  try
    let _ = repl_loop state in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()
