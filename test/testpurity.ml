open Gobba.Types
open Util

module A = Alcotest

let test_pur () =
  checkpurity "fun x y z -> let a = x + (x * 2) in x * a * (y/z) + 2" Numerical;
  checkpurity "fun x y z -> let lazy a = x + (x * 2) in x * a * (y/z) + 2" Numerical;
  check   "(impure $ IO:print 3) >> 4 " (EvtInt 4); 
  check   "impure $ (fun x y z -> let lazy a = x + (x * 2) in x * a * (y/z >> IO:print 4 >> 4) + 2) 2 3 4" (EvtInt 50);
  check   "impure $ IO:print_endline (show 3)" EvtUnit;
  check   "let bad = fun x -> IO:print_endline x in bad"
    (Closure (None, "x",
              (Apply ((Binop (Getkey, (Symbol "IO"), (Symbol "print_endline"))),
                      (Symbol "x"))),
              [("bad",
                (Closure ((Some "bad"), "x",
                          (Apply ((Binop (Getkey, (Symbol "IO"), (Symbol "print_endline"))),
                                  (Symbol "x"))),
                          [])))
              ]
             ));
  check     "fun x -> IO:print_endline x"
    (Closure (None, "x",
              (Apply ((Binop (Getkey, (Symbol "IO"), (Symbol "print_endline"))),
                      (Symbol "x"))),
              []));
  check     "{bad = fun x -> IO:print_endline x}:bad"
    (Closure (None, "x",
              (Apply ((Binop (Getkey, (Symbol "IO"), (Symbol "print_endline"))), (Symbol "x"))),
              [("bad", (Closure ((Some "bad"), "x",
                                 (Apply ((Binop (Getkey, (Symbol "IO"), (Symbol "print_endline"))),
                                         (Symbol "x"))), [])))]));
  check     "impure $ {bad = fun x -> IO:print_endline x}:bad 3" EvtUnit;
  checkfail "{bad = fun x -> IO:print_endline x}:bad 3";
  checkfail "pure $ impure $ IO:print_endline (show 3)";
  checkfail "pure $ IO:print_endline (show 3)";
  checkfail "IO:print_endline (show 3)"



let test_suite = List.map quickcase [
    ("infer purity correctly", test_pur);
  ]