open Gobba.Types
open Util

module A = Alcotest

let plus_one = (Lambda("n", Binop(Plus,Symbol "n", NumInt 1)))

let fib = "fun n -> if n < 2 then n else (fib (n - 1)) + (fib (n - 2))"


let test_constants () =
  check "32" (EvtInt 32);
  check "true" (EvtBool true);
  check "()" (EvtUnit)

let test_apply () =
  check ("let fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  check ("let lazy fib = " ^ fib ^ " in fib 10") (EvtInt 55);
  check "let f = fun n -> n + 1 in f 1" (EvtInt 2);
  checkfail "let f = fun n -> n + 1 in f 1 2";
  checkfail "5 5 "

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
  check "let f = 5 in f" (EvtInt 5);
  check "let fib = fun n -> if n < 2 then n else fib (n - 1) + fib (n - 2) and fiba =  fun n -> if n < 2 then n else fib (n - 1) + fiba (n - 2) in fiba 10"
    (EvtInt 55)

let test_arithmetic () =
  check "4 + 3" (EvtInt 7);
  check "4 - 3" (EvtInt 1);
  check "4 * 3" (EvtInt 12);
  check "5 * 2 * 212 + (134 * 2 - 2 + 1 ) * 1 + 2 * 1 + 2 + 1" (EvtInt 2392);
  check "2 * (4 :+ 2)" (EvtComplex {Complex.re = 8.; Complex.im = 4. });
  checkfail "4 + x";
  checkfail "4 - \"ciao\"";
  checkfail "4 * 'a'"


let test_boolops () =
  check "let x = true in x && false" (EvtBool false);
  check "let x = true in x || false" (EvtBool true);
  check "let x = true in not x" (EvtBool false);
  check "let x = true in not not true" (EvtBool true)

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
    (EvtInt 56);
  check ("((fun x -> x + 1) <=< (let fib = " ^ fib ^ " in fib)) 10")
    (EvtInt 56)

let test_sequence () =
  check "(1 >> 2)"  (EvtInt 2)

let test_lookup () =
  checkfail "notbound";
  check "let lazy a = 1 and lazy b = 2 in a + b" (EvtInt 3)

let test_primitive_abstraction () =
  check "List:head" (Closure (None, "list", ApplyPrimitive(("head", [|"list"|], Pure), [|Symbol "list"|]), []));
  check "List:head [1]" (EvtInt 1);
  check "Dict:insert 3" (Closure (None, "value",
                             (Lambda ("dict",
                                      (ApplyPrimitive (("insert", [|"key";"value";"dict"|], Pure),
                                                       [|(Symbol "key"); (Symbol "value"); (Symbol "dict")|]))
                                     )),
                             [("key",  (EvtInt 3))]))

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