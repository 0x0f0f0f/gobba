open Types
open Eval
open Util
open Interface
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

let run_one command env verbose printres =
  if verbose >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow
      "AST equivalent" "\n%s"
        (show_command command) else ();
  match command with
    | Expr e ->
      let optimized_ast = iterate_optimizer e in
      if optimized_ast = e then () else
        if verbose >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_expr optimized_ast) else ();
      let evaluated = eval optimized_ast env EmptyStack verbose in
      if verbose >= 1 then print_message ~color:T.Green ~loc:(Nowhere) "Result"
      "\t%s" (show_evt evaluated) else ();
      if printres then print_endline (show_unpacked_evt evaluated) else ();
      env
    | Def dl ->
      let (idel, vall) = unzip dl in
      let ovall = (List.map (iterate_optimizer) vall) in
      if ovall = vall then () else
            if verbose >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
              (show_command (Def(zip idel ovall))) else ();
      (bindlist env idel (List.map
        (fun x -> AlreadyEvaluated (eval x env EmptyStack verbose)) ovall))
    | Defrec dl ->
      let odl = (List.map (fun (i,v) -> (i, iterate_optimizer v)) dl) in
      if dl = odl then () else
            if verbose >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
              (show_command (Def(odl))) else ();
      (bindlist env (fst (unzip odl)) (List.map
        (fun (ident, value) ->
          (match value with
          | Lambda (params, fbody) ->
            let rec_env = (bind env ident
              (AlreadyEvaluated (RecClosure(ident, params, fbody, env))))
            in AlreadyEvaluated (RecClosure(ident, params, fbody, rec_env))
          | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
        ) dl))

let rec repl_loop env verbose  =
  try
  let cmd = read_toplevel (wrap_syntax_errors parser) () in
  let _ = repl_loop (run_one cmd env verbose true) verbose in ()
  with
  | End_of_file -> raise End_of_file
  | Error err -> print_error err; ()
  | Sys.Break -> prerr_endline "Interrupted."; ()
  | e -> print_error (Nowhere, "Error", (Printexc.to_string e)); ()

let repl env verbose =
  Sys.catch_break true;
  try
    let _ = repl_loop env verbose in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()
