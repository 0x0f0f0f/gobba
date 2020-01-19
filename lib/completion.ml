open Util
open Types

(** This module provides the completion callback for the linenoise REPL *)


let str_starts_with s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then false else
    let sub = String.sub s1 0 len2 in
    (sub = s2)

let str_ends_with s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 < len2 then false else
    let sub = String.sub s1 (len1 - len2) len2 in
    (sub = s2)

let str_get_rest s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  if len1 <= len2 then "" else
  let len2 = (String.length s2) in
  String.sub s1 len2 (String.length s1 - len2)

(* A list of pairs of directives and their completion hints *)
let directives = [
  ("#pure ();", "#pure () ; (* Enforce only pure computations! *)");
  ("#impure ();", "#impure (); (* Allow impure computations globally! *)");
  ("#uncertain ();", "#uncertain (); (* Reset to the default purity state *)");
  ("#dumppurityenv ();", "#dumppurityenv (); (* Dump the purity of each value in the environment*)");
  ("#dumpenv ();", "#dumpenv (); (* Dump the all values in the environment*)");
  ("#verbose ", "#verbose <int> (* Set the verbosity level *)");
  ("#module ", "#module <string> (* Load and run a file and export declarations to a module *)");
  ("#include ", "#include <string> (* Load and run a file and import the declarations *)");
]

let all_completions = (fstl directives)

(* Extend a mutable list of completion names with the properties from a dictionary
if it exists in the environment, or from the table of primitives *)
let extend_completions_with_dictionary_properties state varnames last_word  =
  (*
TODO Fixme! *)
  if str_ends_with last_word ":" then
    let last_word = String.sub last_word 0 (String.length last_word - 1) in
  (* If the current word is a dictionary in the env + the ':' character
  then add the props to the current completion list*)
    match Dict.get last_word Primitives.table with
    | Some (EvtDict d) ->
      let propertynames = Dict.getkeys d in
      varnames := ((List.map (fun prop -> last_word ^ ":" ^ prop ) propertynames) @ !varnames)
    | _ -> ();
    match Dict.get last_word !state.env with
    | Some (EvtDict d) ->
      let propertynames = Dict.getkeys d in
      varnames := ((List.map (fun prop -> last_word ^ ":" ^ prop ) propertynames) @ !varnames)
    | _ -> ()

let hints_callback state line =
  if line = "" then None else
  try
  (* Try to complete from directives *)
  List.find (fun x -> str_starts_with x line) (sndl directives) |> fun x -> str_get_rest x line
  |> fun x -> Some (x, LNoise.Yellow, true)
  (* If there's no directive, try to complete from the current environment *)
  with Not_found -> try
    (* Get currently defined variable names from the state pointer *)
    let varnames = ref ((Dict.getkeys !state.env) @ (Dict.getkeys Primitives.table)) in
    let last_word = String.split_on_char ' ' line |> last in
    extend_completions_with_dictionary_properties state varnames last_word;
    List.find (fun x -> str_starts_with x last_word) !varnames |> fun x -> str_get_rest x line
    |> fun x -> Some (x, LNoise.Yellow, true)
  with Not_found -> None

let completion_callback state line_so_far ln_completions =
  (* Get currently defined variable names from the state pointer *)
  let varnames = ref ((Dict.getkeys !state.env) @ (Dict.getkeys Primitives.table)) in
  if line_so_far <> "" then
  let last_word = String.split_on_char ' ' line_so_far |> last in
  extend_completions_with_dictionary_properties state varnames last_word;
  List.filter (fun x -> str_starts_with x last_word) (all_completions @ !varnames)
  |> List.iter (LNoise.add_completion ln_completions)

