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
