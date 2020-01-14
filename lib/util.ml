module Dict = struct
  type ('a, 'b)  t = ('a * 'b) list [@@deriving show { with_path = false }, eq, ord]

  (** Search for duplicates *)
  let rec dup_exists = function
    | [] -> false
    | hd::tl -> List.exists ((=) hd) tl || dup_exists tl

  (** Generate an empty dictionary *)
  let empty = fun _ -> []

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

  (** Search and get a key's value (first match) in a list of key-value pairs *)
  let rec get ks l = match l with
    | [] -> None
    | (k, v)::xs -> if k = ks
      then Some v
      else get ks xs

  let rec filter kl l = match l with
    | [] -> []
    | (k, v)::xs -> if List.mem k kl
      then (k, v)::(filter kl xs)
      else (filter kl xs)

  let rec getkeys l = match l with
  | [] -> []
  | (k, _)::xs -> k::(getkeys xs)

  let rec getvalues l = match l with
  | [] -> []
  | (_, v)::xs -> v::(getvalues xs)

  (** Insert a list of keys and a list of values in a dictionary *)
  let rec insertmany env ident_list value_list =
    match (ident_list, value_list) with
    | ([], []) -> env
    | (i::ident_rest, v::value_rest) ->
      insertmany (insert env i v) ident_rest value_rest
    | _ -> failwith "lists are not of equal length"
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

let last l = List.hd (drop ((List.length l) - 1) l)

let fst (a, _) = a
let snd (_, a) = a

let fst3 (a, _, _) = a
let snd3 (_, a, _) = a
let thd3 (_, _, a) = a

let fstl l = List.map fst l
let sndl l = List.map snd l

let fst3l l = List.map fst3 l
let snd3l l = List.map snd3 l
let thd3l l = List.map thd3 l

(** Helper function to unzip a list of couples *)
let unzip l = if l = [] then ([], []) else
    (fstl l, sndl l)

(** Helper function to unzip a list of triples *)
let unzip3 l = if l = [] then ([], [], []) else
    (fst3l l, snd3l l, thd3l l)

(* Zip together two lists with in single list of couples *)
let rec zip l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | (x::xs, y::ys) -> (x,y)::(zip xs ys)
  | _ -> failwith "lists are not of equal length"

(* Zip together two lists with in single list of couples *)
let rec zip3 l1 l2 l3 =
  match (l1, l2, l3) with
  | ([], [], []) -> []
  | (x::xs, y::ys, z::zs) -> (x,y,z)::(zip3 xs ys zs)
  | _ -> failwith "lists are not of equal length"

