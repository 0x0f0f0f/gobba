open Gobba
open Types
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



let eval_one e state = fst (Gobba.Eval.eval_command (Expr e) state Filename.current_dir_name)

let quickcase (descr, case) = A.test_case descr `Quick case

let parse str = match List.hd (Parsedriver.read_one str) with
  | Expr e -> e
  | _ -> failwith "did not expect a definition here"

let checkparse e result = A.(check myexpr) e result (parse e)

let checkparsefail exp = A.check_raises exp (Failure("syntax error"))
  (fun () -> try let _ = (parse exp) in () with _ -> failwith "syntax error")

let check exp expected = A.(check myevt) exp expected (eval_one (parse exp) state)

let checkfail exp  = A.(check_raises) exp (Failure("evaluation error"))
(fun () -> try let _ = (eval_one (parse exp) state) in () with e ->  print_endline
  (Printexc.print_backtrace stderr; Printexc.to_string e); failwith "evaluation error")

let checkpurity exp expected = A.(check mypurity) exp expected (Puritycheck.infer (parse exp) state)

let examples_path = Sys.getenv "GOBBA_EXAMPLES"

let checkprogram fn expected = A.(check myevt) fn expected (fst (Repl.run_file (Filename.concat examples_path fn) state 20 true))