open Gobba
open Gobba.Types
open Cmdliner

let run_gobba verbose program printresult maxstackdepth ist =
  Printexc.record_backtrace true; 
  let state = {
    env = (Util.Dict.empty());
    purityenv = (Util.Dict.empty());
    verbosity = verbose;
    stack = EmptyStack;
    printresult = printresult;
    purity = Uncertain;
  } in
  match program with
  | None -> Repl.repl {state with printresult = true} maxstackdepth ist
  | Some name -> let _ = Repl.run_file name state maxstackdepth ist in ()

let verbose =
  let doc = "If 1, Print AST to stderr after expressions " ^
            "are entered in the REPL. If 2, also print reduction steps" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"VERBOSITY" ~doc)

let maxstackdepth =
  let doc = "The maximum level of nested expressions to print in a stack trace." in
  Arg.(value & opt int 10 & info ["m"; "maxstackdepth"] ~docv:"MAXSTACKDEPTH" ~doc)

let internalst =
  let doc = "To print or not the language's internal stack traces" in
  Arg.(value & flag & info ["internals"] ~doc)

let print_exprs =
  let doc = "If set, print the result of expressions when evaluating a program from file" in
  Arg.(value & flag & info ["p"; "printexprs"] ~doc)

let program =
  let doc = "The program that will be run. If a program is not provided, launch a REPL shell" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROGRAM_FILE" ~doc)

let run_gobba_t = Term.(const run_gobba $ verbose $ program $ print_exprs $ maxstackdepth $ internalst)

let info =
  let doc =  String.map (fun c -> if c = '\n' then ' ' else c )
      {| gobba is a dynamically typed purely functional programming language,
heavily inspired by OCaml, Haskell and Scheme. The REPL can show each reduction
step that is done in evaluating an expression.|} in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <sudo-woodo3@protonmail.com>"
  ] in
  Term.info "gobba" ~version:"0.4.1" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (run_gobba_t, info)