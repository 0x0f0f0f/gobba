open Interface

(** Parse the contents from a file, using a given [parser]. *)
let read_file parser fn =
try
  let fh = open_in fn in
  let lex = Lexing.from_channel fh in
  lex.Lexing.lex_curr_p <- {lex.Lexing.lex_curr_p with Lexing.pos_fname = fn};
  try
    let terms = wrap_syntax_errors parser lex in
    close_in fh;
    terms
  with
    (* Close the file in case of any parsing errors. *)
    Error err -> close_in fh ; raise (Error err)
with
  (* Any errors when opening or closing a file are fatal. *)
  Sys_error msg -> fatal_error "%s" msg

let parser = Parser.file Lexer.token

let rec run_file cmdlst env verbose = match cmdlst with
  | x::xs -> run_file xs (Repl.run_one x env verbose) verbose
  | [] -> ()
