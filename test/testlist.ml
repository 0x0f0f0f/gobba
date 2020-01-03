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

let test_map () =
  checkeval (Apply ((Symbol "map"),
                    [(Lambda (["x"], (Plus ((Integer 1), (Symbol "x")))));
                     (List [Integer 1; Integer 2; Integer 3; Integer 4])]))
    (EvtList [EvtInt 2; EvtInt 3; EvtInt 4; EvtInt 5]);
  checkevalfail (Apply (Symbol "map", [String "fail"; String "fail"; String
                                         "Fail"]));
  checkevalfail (Apply (Symbol "map", [(Lambda (["x"], (Plus ((Integer 1),
                                                              (Symbol "x"))))); (String "x")]))

let test_foldl () =
  checkeval (Apply ((Symbol "foldl"),
                    [(Lambda (["acc"; "x"], (Plus ((Symbol "acc"), (Symbol "x")))));
                     (Integer 0);
                     (List [Integer 1; Integer 2; Integer 3; Integer 4])]))
    (EvtInt 10);
  checkevalfail (Apply (Symbol "foldl", [String "fail"; String "fail"; String
                                           "Fail"; Integer 0]));
  checkevalfail (Apply (Symbol "foldl", [(Lambda (["x"], (Plus ((Integer 1),
                                                                (Symbol "x"))))); (Integer 0); (String "x")]))

let test_filter () =
  checkeval (Apply ((Symbol "filter"),
                    [(Lambda (["x"], (Gt ((Symbol "x"), (Integer 3)))));
                     (List
                        [(Integer 1); (Integer 2); (Integer 3); (Integer 4); (Integer 5);
                         (Integer 4); (Integer 3); (Integer 2); (Integer 1)])
                    ]))
    (EvtList [(EvtInt 4); (EvtInt 5); (EvtInt 4)]);
  checkevalfail (Apply ((Symbol "filter"),
                        [(Lambda (["x"], (Gt ((Symbol "x"), (Integer 3)))));
                         (List
                            [(Integer 1); (Integer 2); (Integer 3); (Integer 4); (Integer 5);
                             (Integer 4); (Integer 3); (Integer 2); (Integer 1)]); Integer 3
                        ]));
  checkevalfail (Apply ((Symbol "filter"),
                        [(Lambda (["x"], (Gt ((Symbol "x"), (Integer 3))))); Integer 3 ]))

let test_concat () =
  check ("[1] @ [2]") (EvtList [EvtInt 1; EvtInt 2])


let test_suite = List.map quickcase [
    ("evaluate list", test_list);
    ("head", test_head);
    ("tail", test_tail);
    ("cons", test_cons);
    ("concat", test_concat);
    ("map", test_map);
    ("foldl", test_foldl);
    ("filter", test_filter);
  ]