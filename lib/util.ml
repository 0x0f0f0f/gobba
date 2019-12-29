open Types

(** Helper function to take the first elements of a list *)
let rec take k xs = match k with
  | 0 -> []
  | k -> match xs with
  | [] -> failwith "take"
  | y::ys -> y :: (take (k - 1) ys)

(** Helper function to drop the first elements of a list *)
let rec drop k xs = match k with
  | 0 -> xs
  | k -> match xs with
  | [] -> failwith "drop"
  | _::ys -> (drop (k - 1) ys)

let fst (a, _) = a
let snd (_, a) = a

let fstl l = List.map fst l
let sndl l = List.map snd l

(** Helper function to unzip a list of couples *)
let unzip l = if l = [] then ([], []) else
  (fstl l, sndl l)

(* *)
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x,y)::(zip xs ys)
  | _ -> failwith "lists are not of equal length"

(* Search for duplicates *)
let rec dup_exist = function
  | [] -> false
  | hd::tl -> List.exists ((=) hd) tl || dup_exist tl

(* Search for duplicates in a list of key-value pairs *)
let rec dup_key_exist = function
  | [] -> false
  | (hk, _)::tl -> List.exists (fun (x,_) -> x = hk) tl || dup_key_exist tl

let rec delete_key ks l = match l with
  | [] -> []
  | (k, v)::xs -> if k = ks
  then delete_key ks xs
  else (k, v)::(delete_key ks xs)

(* Search for key in a list of key-value pairs *)
let rec key_exist ks l = match l with
  | [] -> false
  | (k, _)::xs -> if k = ks
  then true
  else key_exist ks xs

(* Search and get a key's value (first match) in a list of key-value pairs *)
let rec get_key_val ks l = match l with
  | [] -> failwith "not found"
  | (k, v)::xs -> if k = ks
  then v
  else get_key_val ks xs

let rec filter_by_keys kl l = match l with
  | [] -> []
  | (k, v)::xs -> if List.mem k kl
    then (k, v)::(filter_by_keys kl xs)
    else (filter_by_keys kl xs)

(** Generate an empty environment *)
let empty_env : unit -> env_type =
  fun _ -> []

(** Bind a value (evaluated or not, see lazyness) to an identifier, returning a new environment *)
let bind env ident value =
  (ident, value) :: (delete_key ident env)

(** Bind a list of identifiers to a list of values, returning a new environment *)
let rec bindlist env ident_list value_list =
  match (ident_list, value_list) with
  | ([], []) -> env
  | (i::ident_rest, v::value_rest) ->
    bindlist (bind env i v) ident_rest value_rest
  | _ -> raise WrongBindList

