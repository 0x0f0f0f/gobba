open Types

(** Function to generate an empty environment *)
let empty_env : unit -> env_type =
    fun _ -> []

(** Bind an evaluated value to an identifier, returning an environment *)
let bind (env: env_type) (ident: ide) (value: type_wrapper) : env_type =
    (ident, value) :: env

(** Bind a list of identifiers to a list of values, returning an environment *)
let rec bindlist
    (env: env_type)
    (ident_list: ide list)
    (value_list: type_wrapper list)
    : env_type =
    match (ident_list, value_list) with
    | ([], []) -> env
    | (i::ident_rest, v::value_rest) ->
        bindlist (bind env i v) ident_rest value_rest
    | _ -> raise WrongBindList

