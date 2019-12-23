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

let checkparsefail expr = A.check_raises expr (Failure("syntax error"))
  (fun () -> try let _ = (parse expr) in () with _ -> failwith "syntax error")

let checkeval exp expected = A.(check bool) (show_expr exp) true (equal_evt (eval exp
(empty_env ()) EmptyStack false) expected)