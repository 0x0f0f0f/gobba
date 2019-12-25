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
  "hello", EvtString "world"); (EvtString "apple", EvtInt 314)]));
  checkevalfail (Apply(Symbol "insert", [String "doesntexist"; Integer 123; sample_dict; sample_dict]))


let test_delete () =
  checkeval (Apply(Symbol "delete", [String "hello"; sample_dict]))
  (EvtDict([(EvtString "apple", EvtInt 314)]));
  checkevalfail (Apply(Symbol "delete", [String "doesntexist"; sample_dict; sample_dict]));
  checkevalfail (Apply(Symbol "delete", [String "doesntexist"; sample_dict]))

let test_haskey () =
  checkeval (Apply(Symbol "haskey", [String "ciaone"; sample_dict])) (EvtBool false);
  checkeval (Apply(Symbol "haskey", [String "hello"; sample_dict])) (EvtBool true);
  checkevalfail (Apply(Symbol "haskey", [String "doesntexist"; sample_dict; sample_dict]))

let test_getkey () =
  checkeval (Apply(Symbol "getkey", [String "hello"; sample_dict])) (EvtString "world");
  checkevalfail (Apply(Symbol "getkey", [String "doesntexist"; sample_dict; sample_dict]));
  checkevalfail (Apply(Symbol "getkey", [String "doesntexist"; sample_dict]))

let test_map () =
  checkeval (Apply ((Symbol "map"),
    [(Lambda (["x"], (Sum ((Integer 1), (Symbol "x")))));
    (Dict
      [((String "a"), (Integer 1)); ((String "b"), (Integer 2));
      ((String "c"), (Integer 3)); ((String "d"), (Integer 4))])
    ]))
    (EvtDict
      [((EvtString "a"), (EvtInt 2)); ((EvtString "b"), (EvtInt 3));
      ((EvtString "c"), (EvtInt 4)); ((EvtString "d"), (EvtInt 5))]);
  checkevalfail (Apply (Symbol "map", [String "fail"; String "fail"; String "Fail"]))

let test_foldl () =
  checkeval (Apply ((Symbol "foldl"),
   [(Lambda (["acc"; "x"], (Sum ((Symbol "acc"), (Symbol "x")))));
   (Integer 0);
   (Dict
    [((String "a"), (Integer 1)); ((String "b"), (Integer 2));
      ((String "c"), (Integer 3)); ((String "d"), (Integer 4))])
   ]
   )) (EvtInt 10);
   checkevalfail (Apply (Symbol "foldl", [String "fail"; String "fail"; String
   "Fail"; Integer 0]))

let test_filterkeys () =
  checkeval (Apply(Symbol "filterkeys", [List[String "apple"]; sample_dict]))
  (EvtDict [(EvtString "apple", EvtInt 314)]);
  checkevalfail (Apply(Symbol "filterkeys", [List[String "doesntexist"]; sample_dict; sample_dict]))

let test_suite = List.map quickcase [
  ("evaluate dictionary", test_dict);
  ("insert in a dictionary", test_insert);
  ("delete from a dictionary", test_delete);
  ("haskey", test_haskey);
  ("getkey", test_getkey);
  ("map", test_map);
  ("foldl", test_foldl);
  ("filterkeys", test_filterkeys);
(*   ("no duplicate keys", test_duplicate) *)
]