open Minicaml
open Cmdliner

let run_minicaml verbose program =
  match program with
  | None -> Repl.repl (Env.empty_env()) verbose
  | Some name -> File.run_file name verbose


let verbose = 
  let doc = "If 1, Print AST to stderr after expressions " ^
  "are entered in the REPL. If 2, print also reduction steps" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"VERBOSITY" ~doc)

let program =
  let doc = "The program that will be run. If not a program is not provided, launch a REPL shell." in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROGRAM_FILE" ~doc)

let run_minicaml_t = Term.(const run_minicaml $ verbose $ program)

let info =
  let doc = "a small, purely functional interpreted programming language " ^
  "with a didactical purpose. It is based on the Prof. Gianluigi Ferrari and " ^
  "Prof. Francesca Levi's minicaml, an evaluation example to show students " ^
  "attending the Programming 2 course at the University of Pisa how interpreters " ^
  "work. It is an interpreted language with a Caml-like syntax, featuring " ^
  "interchangeable eager and lazy evaluation, a didactical REPL " ^
  "that shows each AST expression and each evaluation step. " in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <sudo-woodo3@protonmail.com>"
  ] in
  Term.info "minicaml" ~version:"0.3.5" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (run_minicaml_t, info)