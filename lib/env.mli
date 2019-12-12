open Types

(** Generate an empty environment *)
val empty_env : unit -> env_type 

(** Bind a value (evaluated or not, see lazyness) to an identifier, returning a new environment *)
val bind : env_type -> ide -> type_wrapper -> env_type

(** Bind a list of identifiers to a list of values, returning a new environment *)
val bindlist : env_type -> ide list -> type_wrapper list -> env_type 
