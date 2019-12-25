open Minicaml.Types
open Minicaml.Repl
open Minicaml.Eval
open Minicaml.Env


module A = Alcotest

let quickcase (descr, case) = A.test_case descr `Quick case

let parse = read_one parser

let checkeq descr fst snd eqfn = A.(check bool) descr true (eqfn fst snd)

let checkparse expr result =
  A.(check bool) expr true (equal_expr (parse expr) result)

let checkparsefail exp = A.check_raises exp (Failure("syntax error"))
  (fun () -> try let _ = (parse exp) in () with _ -> failwith "syntax error")

let checkeval exp expected = A.(check bool) (show_expr exp) true (equal_evt (eval exp
(empty_env ()) EmptyStack false) expected)

let checkevalfail exp = A.(check_raises) (show_expr exp)
(Failure("evaluation error")) (fun () -> try let _ = (eval exp (empty_env ())
EmptyStack false) in () with _ -> failwith "evaluation error")