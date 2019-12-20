open Minicaml.Types
open Minicaml.Repl

module A = Alcotest

let parse = read_one (wrap_syntax_errors parser)

let test_int () =
A.(check bool) "integers" true (equal_expr (parse "3") (Integer(3)))

let test_neg_int () =
  A.(check bool) "negative integers" true (equal_expr (parse "-3") (Integer(-3)));
  A.(check bool) "negative integers" true (equal_expr (parse "-1251356125") (Integer(-1251356125)));
  A.(check bool) "negative integers" true (equal_expr (parse " -32") (Integer(-32)));
  A.(check bool) "negative integers" true (equal_expr (parse "-0") (Integer(0)))

let test_bool () =
  A.(check bool) "true" true (equal_expr (parse "true") (Boolean(true)));
  A.(check bool) "false" true  (equal_expr (parse "false") (Boolean(false)))

let test_arithmetic () =
  A.(check bool) "+" true
    (equal_expr (parse "5 + 3") (Sum(Integer 5, Integer 3)));
  A.(check bool) "-" true
    (equal_expr (parse "x - 123") (Sub(Symbol "x", Integer 123)))


let test_suite = [
  A.test_case "parse integers" `Quick test_int;
  A.test_case "parse negative integers" `Quick test_neg_int;
  A.test_case "booleans" `Quick test_bool;
  A.test_case "arithmetics" `Quick test_arithmetic
]