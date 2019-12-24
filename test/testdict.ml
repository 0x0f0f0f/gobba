open Minicaml.Types
open Util

module A = Alcotest

let sample_dict = (Dict([(String "hello", String "world"); (String "apple", Integer 314)]))

let test_dict () =
  checkeval sample_dict (EvtDict([(EvtString
  "hello", EvtString "world"); (EvtString "apple", EvtInt 314)]))

let test_insert () =
  checkeval (Apply(Symbol "insert", [Integer 123; Integer 456; sample_dict]))
  (EvtDict([(EvtInt 123, EvtInt 456);(EvtString
  "hello", EvtString "world"); (EvtString "apple", EvtInt 314)]))

let test_haskey () =
  checkeval (Apply(Symbol "haskey", [String "ciaone"; sample_dict])) (EvtBool false);
  checkeval (Apply(Symbol "haskey", [String "hello"; sample_dict])) (EvtBool true)


let test_suite = List.map quickcase [
  ("evaluate dictionary", test_dict);
  ("insert in a dictionary", test_insert);
  ("haskey", test_haskey);
(*   ("no duplicate keys", test_duplicate) *)
]