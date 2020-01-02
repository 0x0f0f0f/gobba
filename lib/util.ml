open Types

module Dict = struct 
  (** Search for duplicates *)
  let rec dup_exists = function
    | [] -> false
    | hd::tl -> List.exists ((=) hd) tl || dup_exists tl

  (** Generate an empty dictionary *)
  let empty : unit -> env_type = fun _ -> []

  (** Search for duplicates in a list of key-value pairs *)
  (* let rec dup_exists = function
     | [] -> false
     | (hk, _)::tl -> List.exists (fun (x,_) -> x = hk) tl || dup_exists tl *)

  (** Delete all occurrences of a key in a dictionary *)
  let rec delete ks l = match l with
    | [] -> []
    | (k, v)::xs -> if k = ks
      then delete ks xs
      else (k, v)::(delete ks xs)

  (** Insert a value into a key-value dictionary *)
  let insert d key value = (key, value) :: (delete key d)

  (** Check if a key exists in a list of key-value pairs *)
  let rec exists ks l = match l with
    | [] -> false
    | (k, _)::xs -> if k = ks
      then true
      else exists ks xs

  (** Search and Dict.get a key's value (first match) in a list of key-value pairs *)
  let rec get ks l = match l with
    | [] -> failwith "not found"
    | (k, v)::xs -> if k = ks
      then v
      else get ks xs

  let rec filter kl l = match l with
    | [] -> []
    | (k, v)::xs -> if List.mem k kl
      then (k, v)::(filter kl xs)
      else (filter kl xs)

  (** Insert a list of keys and a list of values in a dictionary *)
  let rec insertmany env ident_list value_list =
    match (ident_list, value_list) with
    | ([], []) -> env
    | (i::ident_rest, v::value_rest) ->
      insertmany (insert env i v) ident_rest value_rest
    | _ -> raise WrongBindList
end

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

(* Zip together two lists with in single list of couples *)
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x,y)::(zip xs ys)
  | _ -> failwith "lists are not of equal length"



