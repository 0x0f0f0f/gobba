open Minicaml.Types
open Util

module A = Alcotest

let test_fibonacci () = checkprogram "fibonacci.ml" (EvtInt 55)

let test_matrix () = checkprogram "matrixmult.ml" (EvtList [EvtList
[EvtInt 9; EvtInt 12; EvtInt 15]; EvtList [EvtInt 19; EvtInt 26; EvtInt 33];
EvtList [EvtInt 29; EvtInt 40; EvtInt 51]])

let test_suite = List.map quickcase [
  ("fibonacci sequence", test_fibonacci);
  ("matrix multiplication", test_matrix);
]