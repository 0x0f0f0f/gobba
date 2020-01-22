(** This module provides the completion callback for the linenoise REPL *)

open Util
open Types

let implode cl = String.concat "" (List.map (String.make 1) cl)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

module Trie = struct
  type t = Start of t list | Stop | Node of char * t list [@@deriving show]

  let empty () = ref @@ Start []

  let rec tree_of_path path = match path with
  | [] -> Stop
  | x::xs -> Node (x, [tree_of_path xs])

  let rec remove_node_from_char c siblings = match siblings with
  | (Node (x, _))::xs when x = c -> xs
  | x::xs -> x::(remove_node_from_char c xs)
  | [] -> []

  let rec get_node_from_char c siblings = match siblings with
  | (Node (x, sub))::_ when x = c -> Node(x, sub)
  | _::xs -> get_node_from_char c xs
  | _ -> raise Not_found

  let rec insert_in_subtree node path =
  match path with
  | [] -> Stop
  | x::xs ->
    (match node with
      | Start _ -> failwith "Malformed Trie Tree"
      | Stop -> Stop
      | Node(c, sub) ->
        try
        let found = get_node_from_char x sub in
        let updated_l = remove_node_from_char x sub in
        let new_node = insert_in_subtree found xs in
        Node(c, new_node::updated_l )
        with Not_found -> Node(c, (tree_of_path path)::sub))

  let insert tree path =
  let cur = List.hd path in
  match tree with
  | Start [] -> Start [tree_of_path path]
  | Start l ->
    (try
    let subtree = get_node_from_char cur l in
    let updated_l = remove_node_from_char cur l in
    let new_node = insert_in_subtree subtree (List.tl path) in
    Start (new_node :: updated_l)
    with Not_found -> Start ((tree_of_path path)::l))
  | _ -> failwith "Malformed Trie Tree"

  let insert_string tree kwd =
  tree := insert !tree (explode kwd)

  let insert_many_strings tree kwdl = List.iter (insert_string tree) kwdl; tree


  let rec find_in_subtree tree path =
  match path with
  | [] -> Some tree
  | x::xs -> (match tree with
    | Stop -> raise Not_found
    | Node(_, sub) ->
      let subtree = get_node_from_char x sub in
      find_in_subtree subtree xs
    | Start _ -> failwith "Malformed Trie Tree")

  let find_subtree tree path =
  let cur = List.hd path in
  match tree with
  | Start [] -> None
  | Start l ->
    (try
    let subtree = get_node_from_char cur l in
    find_in_subtree subtree (List.tl path)
    with Not_found -> None)
  | _ -> failwith "Malformed Trie Tree"

  let rec flatten_subtree_completions tree =
  match tree with
  | Start _ -> []
  | Stop -> []
  | Node (c, [Stop]) -> [[c]]
  | Node(c, sub) ->
    let sublists = List.map flatten_subtree_completions sub in
    let flattened_sublists = List.flatten sublists in
    List.map (fun l -> (c ::  l)) flattened_sublists

  let gen_completions tree kwd =
  let path = explode kwd in
  let x = find_subtree tree path in
  match x with
  | None -> []
  | Some x -> (flatten_subtree_completions x |> List.map implode)
end

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

let tree = Trie.insert_many_strings (Trie.empty ()) all_completions

let varnames = ref []


let hints_callback state line =
  if line = "" then None else begin
    varnames := (Dict.getkeys !state.env) @ (Dict.getkeys Primitives.table);
    let tree = Trie.insert_many_strings tree !varnames in
    let last_word = String.split_on_char ' ' line |> List.map String.trim |> last in
    if last_word <> "" then 
    Trie.gen_completions !tree last_word
    |> List.map (fun x ->  String.sub x 1 (String.length x - 1))
    |> fun x -> Printf.printf "%s" @@ String.concat "," x ; x
    |> fun x -> match x with [] -> None | x::_ -> Some (x, LNoise.Cyan, true)
    else None
  end

let completion_callback line_so_far ln_completions =
  if line_so_far <> "" then
  let last_word = String.split_on_char ' ' line_so_far |> List.map String.trim |> last in
  if last_word <> "" then
  Trie.gen_completions !tree last_word
  |> List.map (fun x ->  String.sub x 1 (String.length x - 1))
  |> List.map (fun x -> line_so_far ^ x)
  |> List.iter (LNoise.add_completion ln_completions)