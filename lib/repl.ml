open Types
open Eval
open Printf
open Lexing
open Errors
open Optimizer

let read_one parser str =
  parser (Lexing.from_string (str ^ "\n"))

let read_toplevel parser () =
    let prompt = "> "
    and prompt_more = "> " in
    print_string prompt ;
    let str = ref (read_line ()) in
      while String.length !str > 0 && !str.[String.length !str - 1] == '\\' do
        print_string prompt_more ;
        str := String.sub !str 0 (String.length !str - 1) ^ "\n" ^ (read_line ())
      done ;
      parser (Lexing.from_string (!str ^ "\n"))

 (** Parser wrapper that catches syntax-related errors and converts them to errors. *)
  let wrap_syntax_errors parser lex =
    try parser lex
    with
      | Failure _ ->
        syntax_error ~loc:(location_of_lex lex) "unrecognised symbol"
      | _ ->
        syntax_error ~loc:(location_of_lex lex) "syntax error"

let print_position lexbuf =
    let pos = lexbuf.lex_curr_p in
    sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol + 1)

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

let repl env verbose =
    Sys.catch_break true;
    try
    while true do
        try
        let command = read_toplevel (wrap_syntax_errors parser) () in
        if verbose then print_message ~loc:(Nowhere) "AST equivalent" "\n%s"
          (show_expr command) else ();
        let optimized_ast = optimize command in
        if optimized_ast = command then () else
          if verbose then print_message ~loc:(Nowhere) "After AST optimization" "\n%s"
          (show_expr optimized_ast) else ();
        let evaluated = eval optimized_ast env EmptyStack verbose in
        if verbose then print_message ~color:T.Green ~loc:(Nowhere) "Result"
        "\t%s" (show_evt evaluated) else ();
        print_endline (show_unpacked_evt evaluated);
        with
            | End_of_file -> raise End_of_file
            | Error err -> print_error err
            | Sys.Break -> prerr_endline "Interrupted."
            | e ->
            print_error (Nowhere, "Semantic Error", (Printexc.to_string e));

    done
    with
      | End_of_file -> prerr_endline "Goodbye!"
