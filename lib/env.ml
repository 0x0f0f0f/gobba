open Types

(** Function to generate an empty environment *)
let empty_env : unit -> env_type =
    fun _ -> []

(** Search for a key in an environment (a (string, value) pair) *)
let rec lookup (env : env_type) (ident : ide) : evt =
    if ident = "" then failwith "invalid identifier" else
    match env with
    | [] -> raise (UnboundVariable ident)
    | (i, e) :: env_rest -> if ident = i then e else lookup env_rest ident

(** Bind a value to an identifier, returning an environment *)
let bind (env: env_type) (ident: ide) (value: evt) : env_type =
    (ident, value) :: env

(** Bind a list of identifiers to a list of values, returning an environment *)
let rec bindlist
    (env: env_type)
    (ident_list: ide list)
    (value_list: evt list)
    : env_type =
    match (ident_list, value_list) with
    | ([], []) -> env
    | (i::ident_rest, v::value_rest) ->
        bindlist (bind env i v) ident_rest value_rest
    | _ -> raise WrongBindList

(*
 Non-functional environment
(* the empty environment *)
let empty_env : unit -> env_type =
    fun _ -> Hashtbl.create 1000

(* Simple but naive environment lookup *)
let lookup
    (env : env_type)
    (ident : ide) : expr =
    if ident = "" then failwith "invalid identifier" else
    Hashtbl.find env ident

(* Bind an identifier to a value *)
let bind
    (env : env_type)
    (ident : ide)
    (value: expr) : unit = if ident = "" then failwith "invalid identifier" else Hashtbl.add
    env ident value

let rec bindlist
    (env : env_type)
    (ident_list : ide list)
    (value_list: expr list)
    : unit = match (ident_list, value_list) with
        | ([], [])  -> ()
        | (ident::ident_rest, value::value_rest) ->
            bind env ident value;
            bindlist env ident_rest value_rest
        | _ -> raise WrongBindList

 *)