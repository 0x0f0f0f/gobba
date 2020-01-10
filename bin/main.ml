open Minicaml
open Minicaml.Types
open Cmdliner

let run_minicaml verbose program printresult javascript prelude =
  let opts = {
    env = (Util.Dict.empty());
    verbosity = verbose;
    stack = EmptyStack;
    printresult = printresult;
    safeness = true;
  } in
  match program with
  | None -> Repl.repl {opts with printresult = true}
  | Some name -> if javascript
    then
      let jscode = File.compile_file name in
      print_string (match prelude with
        | "no" -> jscode
        | "prim" -> (Primitives.jsprelude) ^ jscode
        | "lib" ->  "{" ^ Ramda.ramda ^ "}" ^ (Primitives.jsprelude) ^ jscode
        | _ -> failwith "Invalid prelude type: " ^ prelude)
    else let _ = File.run_file name opts in ()

let verbose =
  let doc = "If 1, Print AST to stderr after expressions " ^
            "are entered in the REPL. If 2, print also reduction steps" in
  Arg.(value & opt int 0 & info ["v"; "verbose"] ~docv:"VERBOSITY" ~doc)

let print_exprs =
  let doc = "If set, print the result of expressions when evaluating a program from file" in
  Arg.(value & flag & info ["p"; "printexprs"] ~doc)

let javascript =
  let doc = "If set, compile the program to JavaScript when reading a program from file" in
  Arg.(value & flag & info ["j"; "javascript"] ~doc)

let prelude =
  let doc = "If set, and minicaml is compiling a program to JavaScript, choose
  which prelude to use. The value \"lib\" includes needed libraries and
  primitives, the value \"prim\" will include only primitives, \"no\" will
  not include a prelude in the program" in
  Arg.(value & opt string "no" & info ["jsprelude"] ~doc)

let program =
  let doc = "The program that will be run. If a program is not provided, launch a REPL shell" in
  Arg.(value & pos 0 (some string) None & info [] ~docv:"PROGRAM_FILE" ~doc)

let run_minicaml_t = Term.(const run_minicaml $ verbose $ program $ print_exprs
$ javascript $ prelude)

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
  Term.info "minicaml" ~version:"0.3.2" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (run_minicaml_t, info)