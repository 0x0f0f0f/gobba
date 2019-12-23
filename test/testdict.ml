open Minicaml.Types
open Util

module A = Alcotest

let sample_dict = (Dict([(String "hello", String "world")]))

let test_dict () =
  checkeval sample_dict (EvtDict([(EvtString
  "hello", EvtString "world")]))

let test_insert () =
  checkeval (DictInsert((Integer 123, Integer 456), sample_dict))
  (EvtDict([(EvtInt 123, EvtInt 456);(EvtString
  "hello", EvtString "world")]))


let test_suite = List.map quickcase [
  ("evaluate dictionary", test_dict);
  ("insert in a dictionary", test_insert);
(*   ("no duplicate keys", test_duplicate) *)
]