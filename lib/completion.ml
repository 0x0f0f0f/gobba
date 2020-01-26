(** This module provides the completion callback for the linenoise REPL *)

open Util

module Trie = struct
  let implode cl = String.concat "" (List.map (String.make 1) cl)

  let explode s =
    let rec exp i l =
      if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []

  type t = Node of bool * (char * t) list

  let empty () = Node(false, [])

  let rec node_of_path path = match path with
    | [] -> Node(true, [])
    | x::[] -> Node(false, [x, Node (true, [])])
    | x::xs -> Node(false, [x, node_of_path xs])

  let rec insert node path =
    match path, node with
    | [], Node(_, sub) -> Node(true, sub)
    | x::xs, Node(isword, sub) ->
      let sub_contains_x = List.exists (fun (c, _) -> c = x) sub in
      let newsub =
        if not sub_contains_x
        then (x, node_of_path xs)::sub
        else
          List.map (fun (c, n) -> if c = x then (c, insert n xs) else (c,n)) sub
        in
      Node(isword, newsub)

  let insert_string node kwd =
  insert node (explode kwd)

  let insert_many_strings tree kwdl =
  let t = ref tree in
  List.iter (fun kwd -> t := insert_string !t kwd) kwdl; !t

  let rec subtree node path =
    match path, node with
    | [],  Node(_, _) -> Some node
    | x::xs, Node(_, sub) ->
      let h = List.find_opt (fun (c, _) -> x = c) sub in
      match h with
      | None -> None
      | Some (_, n) -> subtree n xs

  let rec flatten_subtree node (acc: char list) : char list list =
    match node with
    | Node(isword, sub) ->
      let sublists = List.map (fun (c, n) -> flatten_subtree n (acc @ [c])) sub in
      let flattened_sublists = List.flatten sublists in
      if isword then acc::flattened_sublists else flattened_sublists

  let gen_completions node kwd =
    let path = explode kwd in
    let x = subtree node path in
    match x with
    | None -> []
    | Some x -> (flatten_subtree x [] |> List.map implode)
end

let hints_callback tree line =
  if line = "" then None else begin
    let last_word = String.split_on_char ' ' line |> List.map String.trim |> last in
    if last_word <> "" then
      Trie.gen_completions !tree last_word
      |> fun x -> match x with [] -> None | x::_ -> Some (x, LNoise.Cyan, true)
    else None
  end

let completion_callback tree line_so_far ln_completions =
  if line_so_far <> "" then
    let last_word = String.split_on_char ' ' line_so_far |> List.map String.trim |> last in
    if last_word <> "" then
      Trie.gen_completions !tree last_word
      |> List.map (fun x -> line_so_far ^ x)
      |> List.iter (LNoise.add_completion ln_completions)