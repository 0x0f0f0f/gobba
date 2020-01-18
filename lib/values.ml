open Types

let rec show_unpacked_evt e = match e with
  | EvtUnit -> "()"
  | EvtInt v -> string_of_int v
  | EvtFloat v -> Printf.sprintf "%f" v
  | EvtComplex n -> show_complext n
  | EvtBool v -> string_of_bool v
  | EvtChar c -> String.make 1 c
  | EvtString v -> "\"" ^ (String.escaped v) ^ "\""
  | EvtVect (_, l) -> "[|" ^ (String.concat ", " (Array.map show_unpacked_evt l |> Array.to_list)) ^ "|]"  
  | EvtList l -> "[" ^ (String.concat ", " (List.map show_unpacked_evt l)) ^ "]"
  | EvtDict d -> "{" ^
                 (String.concat ", " 
                    (List.map (fun (x,y) -> x ^ " = " ^ show_unpacked_evt y) d))
                 ^ "}"
  | Closure (name, param, body, _) ->
    (match name with | Some x -> x | None -> "") ^ "(fun " ^ (String.concat " " (param::(Expr.findparams body))) ^ " -> ... )"
  | LazyExpression e -> "<lazy>: " ^ (Expr.show_expr_short e)

(** Function that creates a list with the params of a nested lambda in a Closure *)
let findevtparams l = match l with
  | Closure(_, p, b, _) -> p::(Expr.findparams b)
  | _ -> []