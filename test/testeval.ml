open Minicaml.Types
open Util

module A = Alcotest

let plus_one body =
  (Let([("f", (Lambda(["n"], Sum(Symbol "n", Integer 1))))], body))

let fib body =
  (Letrec ("fib",
   (Lambda (["n"],
      (IfThenElse ((Lt ((Symbol "n"), (Integer 2))), (Symbol "n"),
         (Sum ((Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (Integer 1)))])),
            (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (Integer 2)))]))))
         ))
      )),
   body))

let test_constants () =
  checkeval (Integer 32) (EvtInt 32);
  checkeval (Boolean true) (EvtBool true);
  checkeval (Unit) (EvtUnit)

let test_apply () =
  checkeval (plus_one (Apply(Symbol "f", [Integer 1]))) (EvtInt 2);
  checkeval (fib (Apply(Symbol "fib", [Integer 10]))) (EvtInt 55);
  checkevalfail (plus_one (Apply(Symbol "f", [Integer 1; Integer 2])))

let test_suite = List.map quickcase [
  ("constants", test_constants);
  ("apply", test_apply);
]