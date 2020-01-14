open Minicaml.Types
open Util

module A = Alcotest

let plus_one = (Lambda("n", Plus(Symbol "n", NumInt 1)))

let fib = "fun n -> if n < 2 then n else (fib (n - 1)) + (fib (n - 2))"


let test_constants () =
  checkeval (NumInt 32) (EvtInt 32);
  checkeval (Boolean true) (EvtBool true);
  checkeval (Unit) (EvtUnit)

let test_apply () =
  check ("let rec fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  check ("let rec lazy fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  checkeval (Let(["f", plus_one], (Apply(Symbol "f", NumInt 1)))) (EvtInt 2);
  checkevalfail (Let(["f", plus_one], (Apply (Apply (Symbol "f", NumInt 1), NumInt 2))));
  checkevalfail (Apply(NumInt 5, NumInt 5))

let test_curry () =
  check "let f = fun x y -> x + y in f 3"
    (Closure ("y", (Plus ((Symbol "x"), (Symbol "y"))),
              [("x", (AlreadyEvaluated (EvtInt 3)))], Numerical));
  check ("let rec fibacc = fun a n -> if n < 2 then a + n else (fibacc a (n - 1)) + (fibacc a (n - 2)) in fibacc 2 ")
    (Closure ("n",
              (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))),
                           (Plus ((Symbol "a"), (Symbol "n"))),
                           (Plus
                              ((Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                       (Sub ((Symbol "n"), (NumInt 1))))),
                               (Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                       (Sub ((Symbol "n"), (NumInt 2)))))))
                          )),
              [("a", (AlreadyEvaluated (EvtInt 2)));
               ("fibacc",
                (AlreadyEvaluated
                   (RecClosure ("fibacc", "a",
                                (Lambda ("n",
                                         (IfThenElse ((Lt ((Symbol "n"), (NumInt 2))),
                                                      (Plus ((Symbol "a"), (Symbol "n"))),
                                                      (Plus
                                                         ((Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                                                  (Sub ((Symbol "n"), (NumInt 1))))),
                                                          (Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                                                  (Sub ((Symbol "n"), (NumInt 2)))))))
                                                     ))
                                        )),
                                [], Pure))))
              ], Pure
             ))

let test_let () =
  checkeval (Let([false, "f", NumInt 5], Symbol "f")) (EvtInt 5);
  checkeval (Let([true "f", NumInt 5], Symbol "f")) (EvtInt 5);
  check "let fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) and fiba =  fun n -> if n < 2 then n else fib (n - 1) + fiba (n - 2) in fiba 10"
    (EvtInt 55);
  checkevalfail (Let([false, "f", NumInt 5], Symbol "f"));
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
  check ("((let rec fib = " ^ fib ^ " in fib) >=> (fun x -> x + 1)) 10")
    (EvtInt 56)

let test_sequence () =
  checkeval (Sequence([])) EvtUnit;
  checkeval (Sequence([NumInt 1; NumInt 2])) (EvtInt 2)

let test_lookup () =
  checkevalfail (Symbol "");
  checkevalfail (Symbol "notbound");
  checkeval (Letlazy(["a", NumInt 1; "b", NumInt 2], Symbol "a")) (EvtInt 1)

let test_primitive_abstraction () =
  check "head" (Closure ("a", ApplyPrimitive(("head", 1, Pure), [Symbol "a"]), [], Pure));
  check "head [1]" (EvtInt 1);
  check "insert 3" (Closure ("b",
                             (Lambda ("c",
                                      (ApplyPrimitive (("insert", 3, Pure),
                                                       [(Symbol "a"); (Symbol "b"); (Symbol "c")]))
                                     )),
                             [("a", (AlreadyEvaluated (EvtInt 3)))], Pure))

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