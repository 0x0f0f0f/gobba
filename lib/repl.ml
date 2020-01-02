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

let run_one command opts =
  if opts.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow
      "AST equivalent" "\n%s"
      (show_command command) else ();
  match command with
  | Expr e ->
    let optimized_ast = iterate_optimizer e in
    if optimized_ast = e then () else
    if opts.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_expr optimized_ast) else ();
    let evaluated = eval optimized_ast opts in
    if opts.verbosity >= 1 then print_message ~color:T.Green ~loc:(Nowhere) "Result"
        "\t%s" (show_evt evaluated) else ();
    if opts.printresult then print_endline (show_unpacked_evt evaluated) else ();
    opts.env
  | Def dl ->
    let (idel, vall) = unzip dl in
    let ovall = (List.map (iterate_optimizer) vall) in
    if ovall = vall then () else
    if opts.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_command (Def(zip idel ovall))) else ();
    (Dict.insertmany opts.env idel (List.map
                                      (fun x -> AlreadyEvaluated (eval x opts)) ovall))
  | Defrec dl ->
    let odl = (List.map (fun (i,v) -> (i, iterate_optimizer v)) dl) in
    if dl = odl then () else
    if opts.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_command (Def(odl))) else ();
    (Dict.insertmany opts.env (fst (unzip odl))
       (List.map
          (fun (ident, value) ->
             (match value with
              | Lambda (params, fbody) ->
                let rec_env = (Dict.insert opts.env ident
                                 (AlreadyEvaluated (RecClosure(ident, params, fbody, opts.env))))
                in AlreadyEvaluated (RecClosure(ident, params, fbody, rec_env))
              | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
          ) dl))

let rec repl_loop opts  =
  let loop () =
    let cmd = read_toplevel (wrap_syntax_errors parser) () in
    let _ = repl_loop {opts with env = (run_one cmd opts)} in ()
  in
  try
    loop ()
  with
  | End_of_file -> raise End_of_file
  | Error err -> print_error err; loop ()
  | Sys.Break -> prerr_endline "Interrupted.";
  | e -> print_error (Nowhere, "Error", (Printexc.to_string e)); loop ()

let repl opts =
  Sys.catch_break true;
  try
    let _ = repl_loop opts in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()
