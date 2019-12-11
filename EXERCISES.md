1. Add a primitive stack of integers

```ocaml
type expr =
    (* other types *)
    | Stack of valuelist
    | Push of expr * expr
    | Pop of expr 
    and valuelist = Empty | Val of expr * valuelist

type evt = 
```