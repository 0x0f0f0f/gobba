open Types

let empty_env : unit -> env_type =
  fun _ -> []

let bind env ident value =
  (ident, value) :: env

let rec bindlist env ident_list value_list =
  match (ident_list, value_list) with
  | ([], []) -> env
  | (i::ident_rest, v::value_rest) ->
    bindlist (bind env i v) ident_rest value_rest
  | _ -> raise WrongBindList

