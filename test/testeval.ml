open Gobba.Types
open Util

module A = Alcotest

let plus_one = (Lambda("n", Binop(Plus,Symbol "n", NumInt 1)))

let fib = "fun n -> if n < 2 then n else (fib (n - 1)) + (fib (n - 2))"


let test_constants () =
  checkeval (NumInt 32) (EvtInt 32);
  checkeval (Boolean true) (EvtBool true);
  checkeval (Unit) (EvtUnit)

let test_apply () =
  check ("let fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  check ("let lazy fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  checkeval (Let([false, "f", plus_one], (Apply(Symbol "f", NumInt 1)))) (EvtInt 2);
  checkevalfail (Let([false, "f", plus_one], (Apply (Apply (Symbol "f", NumInt 1), NumInt 2))));
  checkevalfail (Apply(NumInt 5, NumInt 5))

let test_curry () =
  check "let f = fun x y -> x + y in f 3"
    (Closure
       (None, "y", (Binop(Plus,(Symbol "x"), (Symbol "y"))),
        [("x", (EvtInt 3));
         ("f",
          (Closure ((Some "f"), "x",
                    (Lambda ("y", (Binop(Plus,(Symbol "x"), (Symbol "y"))))), [])))
        ]));
  check ("let fibacc = fun a n -> if n < 2 then a + n else (fibacc a (n - 1)) + (fibacc a (n - 2)) in fibacc 2 ")
    (Closure (None, "n",
              (IfThenElse ((Binop(Lt,(Symbol "n"), (NumInt 2))),
                           (Binop(Plus,(Symbol "a"), (Symbol "n"))),
                           (Binop(Plus,(Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                       (Binop(Sub,(Symbol "n"), (NumInt 1))))),
                               (Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                       (Binop(Sub,(Symbol "n"), (NumInt 2)))))))
                          )),
              [("a", (EvtInt 2));
               ("fibacc",
                (Closure ((Some "fibacc"), "a",
                          (Lambda ("n",
                                   (IfThenElse ((Binop(Lt,(Symbol "n"), (NumInt 2))),
                                                (Binop(Plus,(Symbol "a"), (Symbol "n"))),
                                                (Binop(Plus,(Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                                            (Binop(Sub,(Symbol "n"), (NumInt 1))))),
                                                    (Apply ((Apply ((Symbol "fibacc"), (Symbol "a"))),
                                                            (Binop(Sub,(Symbol "n"), (NumInt 2)))))))
                                               ))
                                  )),
                          [])))
              ]
             ))

let test_let () =
  checkeval (Let([false, "f", NumInt 5], Symbol "f")) (EvtInt 5);
  checkeval (Let([true, "f", NumInt 5], Symbol "f")) (EvtInt 5);
  check "let fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) and fiba =  fun n -> if n < 2 then n else fib (n - 1) + fiba (n - 2) in fiba 10"
    (EvtInt 55)

let test_arithmetic () =
  checkeval (Binop(Plus,NumInt 4, NumInt 3)) (EvtInt 7);
  checkeval (Binop(Sub,NumInt 4, NumInt 3)) (EvtInt 1);
  checkeval (Binop(Mult,NumInt 4, NumInt 3)) (EvtInt 12);
  check "5 * 2 * 212 + (134 * 2 - 2 + 1 ) * 1 + 2 * 1 + 2 + 1" (EvtInt 2392);
  checkevalfail (Binop(Plus,NumInt 4, String "x"))

let test_boolops () =
  checkeval (Binop(And,Boolean true, Boolean false)) (EvtBool false);
  checkeval (Binop(Or,Boolean true, Boolean false)) (EvtBool true);
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
  check ("((let fib = " ^ fib ^ " in fib) >=> (fun x -> x + 1)) 10")
    (EvtInt 56)

let test_sequence () =
  checkeval (Sequence(NumInt 1, NumInt 2)) (EvtInt 2)

let test_lookup () =
  checkevalfail (Symbol "");
  checkevalfail (Symbol "notbound");
  checkeval (Let([true, "a", NumInt 1; true, "b", NumInt 2], Symbol "a")) (EvtInt 1)

let test_primitive_abstraction () =
  check "head" (Closure (None, "a", ApplyPrimitive(("head", 1, Pure), [Symbol "a"]), []));
  check "head [1]" (EvtInt 1);
  check "Dict:insert 3" (Closure (None, "b",
                             (Lambda ("c",
                                      (ApplyPrimitive (("insert", 3, Pure),
                                                       [(Symbol "a"); (Symbol "b"); (Symbol "c")]))
                                     )),
                             [("a",  (EvtInt 3))]))

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