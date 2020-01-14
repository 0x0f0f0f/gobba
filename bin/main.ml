open Minicaml
open Minicaml.Types
open Cmdliner

let run_minicaml verbose program printresult =
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
  | None -> Repl.repl {state with printresult = true}
  | Some name -> let _ = File.run_file name state in ()

let verbose =
  let doc = "If 1, Print AST to stderr after expressions " ^
            "are entered in the REPL. If 2, print also reduction steps" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"VERBOSITY" ~doc)

let print_exprs =
  let doc = "If set, print the result of expressions when evaluating a program from file" in
  Arg.(value & flag & info ["p"; "printexprs"] ~doc)

let program =
  let doc = "The program that will be run. If a program is not provided, launch a REPL shell" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROGRAM_FILE" ~doc)

let run_minicaml_t = Term.(const run_minicaml $ verbose $ program $ print_exprs )

let info =
  let doc =  String.map (fun c -> if c = '\n' then ' ' else c )
      {| minicaml is a small, purely functional interpreted programming language. Parsing
and lexing are done with menhir and ocamllex The REPL can show each reduction
step that is done in evaluating an expression.|} in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <sudo-woodo3@protonmail.com>"
  ] in
  Term.info "minicaml" ~version:"0.3.3" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (run_minicaml_t, info)