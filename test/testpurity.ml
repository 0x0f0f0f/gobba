open Minicaml.Types
open Util

module A = Alcotest

let test_num () =
  checkpurity "fun x y z -> let a = x + (x * 2) in x * a * (y/z) + 2" Numerical;
  checkpurity "fun x y z -> let lazy a = x + (x * 2) in x * a * (y/z) + 2" Numerical;
  check "impure $ (fun x y z -> let lazy a = x + (x * 2) in x * a * (y/z; print \"ciao\"; 4) + 2) 2 3 4"
    (EvtInt 50);
  check     "impure $ print_endline (show 3)" EvtUnit;
  checkfail "pure $ impure $ print_endline (show 3)";
  checkfail "pure $ print_endline (show 3)";
  checkfail "print_endline (show 3)"

let test_suite = List.map quickcase [
    ("infer purity correctly", test_num);
  ]