open Types
open Eval
open Util
open Interface
open Optimizer

let read_one parser str =
  parser (Lexing.from_string (str ^ "\n"))

let read_toplevel parser () =
  let prompt = "> " in
  let str = Ocamline.read
    ~prompt:prompt
    ~brackets:[('(', ')'); ('[',']');  ('{','}')]
    ~strings:['"']
    ";;" in
  parser (Lexing.from_string (str))

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

let run_one command state =
  if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow
      "AST equivalent" "\n%s"
      (show_command command) else ();
  match command with
  | Expr e ->
    let optimized_ast = iterate_optimizer e in
    if optimized_ast = e then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_expr optimized_ast) else ();
    let evaluated = eval optimized_ast state in
    if state.verbosity >= 1 then print_message ~color:T.Green ~loc:(Nowhere) "Result"
        "\t%s" (show_evt evaluated) else ();
    if state.printresult then
      print_endline 
        ("result: " ^ (show_unpacked_evt evaluated)
         ^ " - " ^ (show_tinfo (Typecheck.typeof evaluated)))
    else ();
    (evaluated, state)
  | Def dl ->
    let (idel, vall) = unzip dl in
    let ovall = (List.map (iterate_optimizer) vall) in
    if ovall = vall then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_command (Def(zip idel ovall))) else ();
    let newenv = Dict.insertmany state.env idel
        (List.map (fun x -> AlreadyEvaluated (eval x state)) ovall) in
    (EvtUnit, { state with env = newenv } )
  | Defrec dl ->
    let odl = (List.map (fun (i,v) -> (i, iterate_optimizer v)) dl) in
    if dl = odl then () else
    if state.verbosity >= 1 then print_message ~loc:(Nowhere) ~color:T.Yellow "After AST optimization" "\n%s"
        (show_command (Def(odl))) else ();
    let newenv = Dict.insertmany state.env (fst (unzip odl))
        (List.map
           (fun (ident, value) ->
              (match value with
               | Lambda (params, fbody) ->
                 let rec_env = (Dict.insert state.env ident
                                  (AlreadyEvaluated (RecClosure(ident, params, fbody, state.env))))
                 in AlreadyEvaluated (RecClosure(ident, params, fbody, rec_env))
               | _ -> raise (TypeError "Cannot define recursion on non-functional values"))
           ) dl) in
    (EvtUnit, { state with env = newenv } )

let rec repl_loop state  =
  let loop () =
    let cmd = read_toplevel (wrap_syntax_errors parser) () in
    let _, newstate = run_one cmd state in
    let _ = repl_loop newstate in ()
  in
  try
    loop ()
  with
  | End_of_file -> raise End_of_file
  | Error err -> print_error err; repl_loop state
  | Sys.Break -> prerr_endline "Interrupted."; repl_loop state
  | e -> print_error (Nowhere, "Error", (Printexc.to_string e)); repl_loop state

let repl state =
  Sys.catch_break true;
  try
    let _ = repl_loop state in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()
