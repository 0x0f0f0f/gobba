open Minicaml.Types
open Util

module A = Alcotest

let plus_one = (Lambda(["n"], Plus(Symbol "n", NumInt 1)))

let fib = "fun n -> if n < 2 then n else (fib (n - 1)) + (fib (n - 2))"


let test_constants () =
  checkeval (NumInt 32) (EvtInt 32);
  checkeval (Boolean true) (EvtBool true);
  checkeval (Unit) (EvtUnit)

let test_apply () =
  check ("let rec fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  check ("let rec lazy fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  checkeval (Let(["f", plus_one], (Apply(Symbol "f", [NumInt 1])))) (EvtInt 2);
  checkevalfail (Let(["f", plus_one], (Apply(Symbol "f", [NumInt 1; NumInt 2]))));
  checkevalfail (Apply(NumInt 5, [NumInt 5]))
let test_curry () =
  checkeval (Let ([("f", (Lambda (["x"; "y"], (Plus ((Symbol "x"), (Symbol "y"))))))],
    (Apply ((Symbol "f"), [(NumInt 3)]))))
  (Closure (["y"], (Plus ((Symbol "x"), (Symbol "y"))),
    [("x", (AlreadyEvaluated (EvtInt 3)))]));
  checkeval (Letrec ("fib",
   (Lambda (["a"; "n"],
      (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))), (Symbol "n"),
         (Plus (
            (Plus ((Symbol "a"),
               (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (NumInt 1)))]))
               )),
            (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (NumInt 2)))]))))
         ))
      )),
   (Apply ((Symbol "fib"), [(NumInt 2)]))))
  (RecClosure ("fib", ["n"],
   (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))), (Symbol "n"),
      (Plus (
         (Plus ((Symbol "a"),
            (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (NumInt 1)))])))),
         (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (NumInt 2)))]))))
      )),
   [("a", (AlreadyEvaluated (EvtInt 2)));
     ("fib",
      (AlreadyEvaluated
         (RecClosure ("fib", ["a"; "n"],
            (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))), (Symbol "n"),
               (Plus (
                  (Plus ((Symbol "a"),
                     (Apply ((Symbol "fib"),
                        [(Sub ((Symbol "n"), (NumInt 1)))]))
                     )),
                  (Apply ((Symbol "fib"), [(Sub ((Symbol "n"), (NumInt 2)))]
                     ))
                  ))
               )),
            []))))
     ]
   ))

let test_let () =
  checkeval (Let(["f", NumInt 5], Symbol "f")) (EvtInt 5);
  checkeval (Letlazy(["f", NumInt 5], Symbol "f")) (EvtInt 5);
  checkevalfail (Letrec("f", NumInt 5, Symbol "f"));
  checkevalfail (Letreclazy("f", NumInt 5, Symbol "f"))

let test_arithmetic () =
  checkeval (Plus(NumInt 4, NumInt 3)) (EvtInt 7);
  checkeval (Sub (NumInt 4, NumInt 3)) (EvtInt 1);
  checkeval (Mult(NumInt 4, NumInt 3)) (EvtInt 12);
  check "5 * 2 * 212 + (134 * 2 - 2 + 1 ) * 1 + 2 * 1 + 2 + 1" (EvtInt 2392);
  checkevalfail (Plus (NumInt 4, String "x"))

let test_boolops () =
  checkeval (And (Boolean true, Boolean false)) (EvtBool false);
  checkeval (Or (Boolean true, Boolean false)) (EvtBool true);
  checkeval (Not (Boolean true)) (EvtBool false);
  checkeval (Not (Not (Boolean true))) (EvtBool true)

let test_comparisons () =
  check "5 = 5" (EvtBool true);
  check "5 = \"x\"" (EvtBool false);
  check "9 > 3" (EvtBool true);
  check "9 < 3" (EvtBool false);
  check "5 >= 5" (EvtBool true);
  check "5 >= 4" (EvtBool true);
  check "5 >= 6" (EvtBool false);
  check "5 <= 5" (EvtBool true);
  check "5 <= 4" (EvtBool false);
  check "5 <= 6" (EvtBool true)

let test_pipe () =
  checkeval
  (Let (
   [("f",
     (Pipe (
        (Letrec ("fib",
           (Lambda (["n"],
              (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))), (Symbol "n"),
                 (Plus (
                    (Apply ((Symbol "fib"),
                       [(Sub ((Symbol "n"), (NumInt 1)))])),
                    (Apply ((Symbol "fib"),
                       [(Sub ((Symbol "n"), (NumInt 2)))]))
                    ))
                 ))
              )),
           (Symbol "fib"))),
        (Lambda (["x"], (Plus ((Symbol "x"), (NumInt 1))))))))
     ],
   (Apply (Symbol "f", [NumInt 10]))))
  (EvtInt 56)

let test_sequence () =
  checkeval (Sequence([])) EvtUnit;
  checkeval (Sequence([NumInt 1; NumInt 2])) (EvtInt 2)

let test_lookup () =
  checkevalfail (Symbol "");
  checkevalfail (Symbol "notbound");
  checkeval (Letlazy(["a", NumInt 1; "b", NumInt 2], Symbol "a")) (EvtInt 1)

let test_primitive_abstraction () = 
   check "head"  (PrimitiveAbstraction ("head", 1, [], Pure));
   check "head [1]" (EvtInt 1);
   check "insert 3" (Closure (["b"; "c"],
   (Apply ((Symbol "insert"), [(Symbol "a"); (Symbol "b"); (Symbol "c")])),
   [("a", (AlreadyEvaluated (EvtInt 3)))]))

let test_suite = List.map quickcase [
  ("constants", test_constants);
  ("arithmetics", test_arithmetic);
  ("boolean operators", test_boolops);
  ("comparisons", test_comparisons);
  ("let", test_let);
  ("apply", test_apply);
  ("curry", test_curry);
  ("pipe", test_pipe);
  ("sequence", test_sequence);
  ("lookup", test_lookup);
  ("primitives abstraction", test_primitive_abstraction);
]