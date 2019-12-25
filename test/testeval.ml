open Minicaml.Types
open Util

module A = Alcotest

let test_constants () =
  checkeval (Integer 32) (EvtInt 32);
  checkeval (Boolean true) (EvtBool true);
  checkeval (Unit) (EvtUnit)

let test_suite = List.map quickcase [
  ("constants", test_constants);
]