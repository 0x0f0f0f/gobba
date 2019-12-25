open Minicaml.Types
open Util

module A = Alcotest

let sample_list = (List([String "hello"; String "world"; String "apple"; Integer 314]))
let sample_list_evt = (EvtList([EvtString "hello"; EvtString "world"; EvtString "apple"; EvtInt 314]))

let test_list () =
  checkeval sample_list sample_list_evt

let test_head () =
  checkeval (Apply(Symbol "head", [sample_list])) (EvtString "hello");
    checkevalfail (Apply(Symbol "head", [List([]); List([])]));
  checkevalfail (Apply(Symbol "head", [List([])]))

let test_tail () =
  checkeval (Apply(Symbol "tail", [sample_list])) (EvtList([EvtString "world"; EvtString "apple"; EvtInt 314]));
  checkevalfail (Apply(Symbol "tail", [List([]); List([])]));
  checkevalfail (Apply(Symbol "tail", [List([])]))

let test_cons () =
  checkeval (Cons(String "hello", List [])) (EvtList [EvtString "hello"]);
  checkeval (Cons(String "hello", List [String "world"])) (EvtList [EvtString
  "hello"; EvtString "world"])

let test_suite = List.map quickcase [
  ("evaluate list", test_list);
  ("head", test_head);
  ("tail", test_tail);
  ("cons", test_cons);
]