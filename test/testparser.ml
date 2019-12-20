open Minicaml.Types
open Util

module A = Alcotest

let test_int () =
A.(check bool) "integers" true (equal_expr (parse "3") (Integer(3)))

let test_neg_int () =
  checkparse "-3" (Integer(-3));
  checkparse "-1251356125" (Integer(-1251356125));
  checkparse "-32" (Integer(-32));
  checkparse "0" (Integer(0))

let test_bool () =
  checkparse "true" (Boolean(true));
  checkparse "false" (Boolean(false))

let test_arithmetic () =
  checkparse "5 + 3" (Sum(Integer 5, Integer 3));
  checkparse "x - 123" (Sub(Symbol "x", Integer 123));
  checkparse "x * 123" (Mult(Symbol "x", Integer 123));
  checkparse "1235 + 2345 * (123 - 2) + 11"
    (Sum ((Sum ((Integer 1235),
      (Mult ((Integer 2345), (Sub ((Integer 123), (Integer 2))))))),
      (Integer 11)));
  checkparse "1234 + -32" (Sum ((Integer 1234), (Integer(-32))));
  checkparsefail "1234 +- 32";
  checkparsefail "1234 /- 32";
  checkparsefail "1234 + + 32"

let test_random_hell () =
  checkparse
  "[(20; (); ([0; ([]; []; [1]; let rec f = fun n -> if n < 2 then n else f(n -
  1) in f 3)])); [30; 40; 50]; 2]"
  (List (ListValue ((Sequence [(Integer 20); Unit; (List (ListValue ((Integer
  0), (ListValue ((Sequence [(List EmptyList); (List EmptyList);
  (List (ListValue ((Integer 1), EmptyList))); (Letrec ("f", (Lambda (["n"],
  (IfThenElse ((Lt ((Symbol "n"), (Integer 2))),
  (Symbol "n"), (Apply ((Symbol "f"), [(Sub ((Symbol "n"), (Integer 1)))])))))),
  (Apply ((Symbol "f"), [(Integer 3)]))))]), EmptyList)))))]), (ListValue (
  (List (ListValue ((Integer 30), (ListValue ((Integer 40),
  (ListValue ((Integer 50), EmptyList))))))), (ListValue ((Integer 2), EmptyList)))))))


let test_misc_functions () =
  checkparse
  "let rec fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) in fib 5"
  (Letrec ("fib",
   (Lambda (["n"],
      (IfThenElse ((Lt ((Symbol "n"), (Integer 2))), (Symbol "n"),
         (Sum ((Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (Integer 1)))])),
            (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (Integer 2)))]))))
         ))
      )),
   (Apply ((Symbol "fib"), [(Integer 5)]))));
  checkparse "let rec fact = fun n -> if n < 2 then n else n * fact (n - 1) in fact 20"
  (Letrec ("fact",
   (Lambda (["n"],
      (IfThenElse ((Lt ((Symbol "n"), (Integer 2))), (Symbol "n"),
         (Mult ((Symbol "n"),
            (Apply ((Symbol "fact"), [(Sub ((Symbol "n"), (Integer 1)))]))))
         ))
      )),
   (Apply ((Symbol "fact"), [(Integer 20)]))))

let test_suite = List.map quickcase [
  ("parse integers", test_int);
  ("parse negative integers", test_neg_int);
  ("booleans", test_bool);
  ("arithmetics", test_arithmetic);
  ("miscellaneous functions", test_misc_functions)
]