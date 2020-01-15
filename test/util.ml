open Minicaml
open Types
open Repl
open Eval
open Util

module A = Alcotest

let state = {
  env = (Dict.empty ());
  purityenv = (Dict.empty ());
  verbosity = 0;
  printresult = false;
  stack = EmptyStack;
  purity = Uncertain;
}

let myevt = Alcotest.testable pp_evt equal_evt
let myexpr = Alcotest.testable pp_expr equal_expr
let mypurity = Alcotest.testable pp_puret equal_puret

let eval_one e state =
  let p = Puritycheck.infer e state in
  Printf.eprintf "Purity is %s\n%!" (show_puret p);
  eval e state;;

let quickcase (descr, case) = A.test_case descr `Quick case

let parse str = match (read_one parser str) with
  | Expr e -> e
  | _ -> failwith "did not expect a definition here"

let checkparse e result = A.(check myexpr) e result (parse e)

let checkparsefail exp = A.check_raises exp (Failure("syntax error"))
  (fun () -> try let _ = (parse exp) in () with _ -> failwith "syntax error")

let checkeval exp expected = A.(check myevt) (show_expr exp) expected (eval exp state)

let checkevalfail exp = A.(check_raises) (show_expr exp)
(Failure("evaluation error")) (fun () -> try let _ = (eval exp state) in () with _ -> failwith "evaluation error")

let check exp expected = A.(check myevt) exp expected (eval_one (parse exp) state)

let checkfail exp  = A.(check_raises) exp (Failure("evaluation error"))
(fun () -> try let _ = (eval_one (parse exp) state) in () with e ->  print_endline (Printexc.to_string e); failwith "evaluation error")

let checkpurity exp expected = A.(check mypurity) exp expected (Puritycheck.infer (parse exp) state)

let examples_path = Sys.getenv "MINICAML_EXAMPLES"

let checkprogram fn expected = A.(check myevt) fn expected (fst (Repl.run_file (Filename.concat examples_path fn) state 20 true))