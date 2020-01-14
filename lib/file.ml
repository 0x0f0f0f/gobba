open Types

(** Parse the contents from a file, using a given [parser]. *)
let read_file parser fn =
  try
    if not (Sys.file_exists fn) then iraise (FileNotFoundError fn) else
      let fh = open_in fn in
      let lex = Lexing.from_channel fh in
      lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
      try
        let terms = parser lex in
        close_in fh;
        terms
      with
      (* Close the file in case of any parsing errors. *)
        e -> close_in fh ; iraise (SyntaxError (Printexc.print_backtrace stderr; Printexc.to_string e))
  with
  (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> iraise (Fatal msg)

let parser = Parser.file Lexer.token

let rec run_file_list cmdlst state = match cmdlst with
  | x::xs ->
    let result, newstate = Repl.run_one x state in
    [result]::run_file_list xs newstate
  | [] -> []

let run_file fn state =
  try
  run_file_list (read_file parser fn) state
  with
  | InternalError err -> print_error err; []
  | Sys.Break -> prerr_endline "Interrupted."; []
  | e -> print_error (Nowhere, Fatal (Printexc.to_string e), state.stack); []
