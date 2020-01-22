open Types
open Errors

(** Parse and evaluate a gobba expression from a string, in a default state,
returning a couple consisting of an evaluated value (of type evt), that you can
unpack using the [Gobba.Typecheck.unpack_<name>], and the new state resulting
from the command evaluation. This is useful for embedding gobba in your ocaml
programs.
@param str The gobba string to parse and evaluate
@param dirscope A directory path: where gobba will look for files imported with a relative path. Defaults to the current directory.
@param state Specifies the current computation state with options. Uses the default state (with an empty environment) if not provided
@return A couple containing the result of the expression evaluation and the resulting state.
*)
let run_string str ?(dirscope=(Filename.current_dir_name)) ?(state=default_evalstate) () =
  let ast = Parsedriver.read_one str in
  Eval.eval_command (List.hd ast) state dirscope


(* A list of pairs of directives and their completion hints *)
let directives = [
  "#pure ();";
  "#impure ();";
  "#dumppurityenv ();";
  "#dumpenv ();";
  "#verbose ";
  "#module ";
  "#include ";
]

let tree = ref @@ Completion.Trie.insert_many_strings (Completion.Trie.empty ()) directives

module StringSet = Set.Make(String)

let rec gen_keywords_from_env env =
  match env with
  | (n, EvtDict d)::xs ->  n :: (gen_keywords_from_env d |> List.map (fun v -> n ^ ":" ^ v )) @ gen_keywords_from_env xs
  | (n, _)::xs -> n::(gen_keywords_from_env xs)
  | [] -> []

let varnames = ref @@ StringSet.of_list @@ gen_keywords_from_env Primitives.table

(** Read a line from the CLI using ocamline and parse it *)
let read_toplevel () =
  let prompt = "> " in
  let str = Ocamline.read
      ~prompt:prompt
      ~brackets:[('(', ')'); ('[',']');  ('{','}')]
      ~strings:['"']
      ~delim:";"
      ~hints_callback:(Completion.hints_callback tree)
      ~completion_callback:(Completion.completion_callback tree)
      ~history_loc:(Filename.concat (Unix.getenv "HOME") ".gobba-history") () in
  Parsedriver.read_one str

let rec repl_loop state maxdepth internalst =
  while true do
    try
      varnames := StringSet.add_seq (gen_keywords_from_env !state.env |> List.to_seq) !varnames ;
      tree := Completion.Trie.insert_many_strings !tree @@ StringSet.elements !varnames;
      let cmd = List.hd (read_toplevel ()) in
      state := snd (Eval.eval_command cmd !state (Filename.current_dir_name));
    with
    | End_of_file -> raise End_of_file
    | InternalError err ->
      if internalst then Printexc.print_backtrace stderr;
      print_error err;
      print_stacktrace err maxdepth;
      repl_loop state maxdepth internalst
    | Sys.Break ->
      if internalst then Printexc.print_backtrace stderr;
      prerr_endline "Interrupted.";
      repl_loop state maxdepth internalst
    | e ->
      if internalst then Printexc.print_backtrace stderr;
      print_error (Nowhere, (Fatal (Printexc.to_string e)), !state.stack);
      repl_loop state maxdepth internalst
  done


let repl state maxstackdepth internalst =
  Sys.catch_break true;
  let mstate = ref state in
  try
    let _ = repl_loop mstate maxstackdepth internalst in ()
  with End_of_file -> prerr_endline "Goodbye!"; ()


let run_file fn state maxstackdepth internalst =
  try
    Eval.eval_command_list (Parsedriver.read_file fn) state (Filename.dirname fn)
  with
  | InternalError err ->
    if internalst then Printexc.print_backtrace stderr;
    print_error err;
    print_stacktrace err maxstackdepth;
    (EvtBool false, state)
  | Sys.Break ->
    if internalst then Printexc.print_backtrace stderr;
    prerr_endline "Interrupted.";
    (EvtBool false, state)
  | e ->
    if internalst then Printexc.print_backtrace stderr;
    print_error (Nowhere, Fatal (Printexc.to_string e), state.stack);
    (EvtBool false, state)
