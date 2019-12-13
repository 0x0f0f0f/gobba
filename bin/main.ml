open Minicaml
open Cmdliner

let run_repl () = Repl.repl (Env.empty_env())

let run_repl_t = Term.(const run_repl)

let info =
  let doc = "a small, purely functional interpreted programming language" ^
  "with a didactical purpose. It is based on the Prof. Gianluigi Ferrari and" ^
  "Prof. Francesca Levi's minicaml, an evaluation example to show students" ^
  "attending the Programming 2 course at the University of Pisa how interpreters" ^
  "work. It is an interpreted language with a Caml-like syntax, featuring" ^
  "interchangeable eager and lazy evaluation, a didactical REPL" ^
  "that shows each AST expression and each evaluation step." in
  let man = [
    `S Manpage.s_bugs;
    `P "Email bug reports to <sudo-woodo3@protonmail.com>"
  ] in
  Term.info "minicaml" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (run_repl_t, info)