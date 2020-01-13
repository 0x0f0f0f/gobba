open Minicaml.Types
open Util

module A = Alcotest

let sample_list = (List([String "hello"; String "world"; String "apple"; NumInt 314]))
let sample_list_evt = (EvtList([EvtString "hello"; EvtString "world"; EvtString "apple"; EvtInt 314]))

let test_list () =
  checkeval sample_list sample_list_evt

let test_head () =
  checkeval (Apply(Symbol "head", sample_list)) (EvtString "hello");
  checkevalfail (apply_from_exprlist [List([]); List([])] (Symbol "tail"));
  checkevalfail (apply_from_exprlist [List([])] (Symbol "tail"))

let test_tail () =
  checkeval (Apply(Symbol "tail", sample_list)) (EvtList([EvtString "world"; EvtString "apple"; EvtInt 314]));
  checkevalfail (apply_from_exprlist [List([]); List([])] (Symbol "tail"));
  checkevalfail (Apply(Symbol "tail", List([])))

let test_cons () =
  checkeval (Cons(String "hello", List [])) (EvtList [EvtString "hello"]);
  checkeval (Cons(String "hello", List [String "world"])) (EvtList [EvtString
                                                                      "hello"; EvtString "world"])

let test_map () =
  checkeval (apply_from_exprlist
               [(Lambda ("x", (Plus ((NumInt 1), (Symbol "x")))));
                (List [NumInt 1; NumInt 2; NumInt 3; NumInt 4])] (Symbol "map"))
    (EvtList [EvtInt 2; EvtInt 3; EvtInt 4; EvtInt 5]);
  checkevalfail (apply_from_exprlist [String "fail"; String "fail"; String
                                        "Fail"] (Symbol "map"));
  checkevalfail (apply_from_exprlist [(Lambda ("x", (Plus ((NumInt 1),
                                                           (Symbol "x"))))); (String "x")] (Symbol "map"))

let test_foldl () =
  check "foldl (fun acc x -> acc + x) 0 [1;2;3;4]"
    (EvtInt 10);
  checkfail "foldl \"fail\" \"fail\" \"fail\" 0";
  checkfail "foldl (fun x -> 1 + x) 0 \"x\""

let test_filter () =
  checkeval
    (apply_from_exprlist
       [(Lambda ("x", (Gt ((Symbol "x"), (NumInt 3)))));
        (List
           [(NumInt 1); (NumInt 2); (NumInt 3); (NumInt 4); (NumInt 5);
            (NumInt 4); (NumInt 3); (NumInt 2); (NumInt 1)])
       ] (Symbol "filter"))
    (EvtList [(EvtInt 4); (EvtInt 5); (EvtInt 4)]);
  checkevalfail
    (apply_from_exprlist
       [(Lambda ("x", (Gt ((Symbol "x"), (NumInt 3)))));
        (List
           [(NumInt 1); (NumInt 2); (NumInt 3); (NumInt 4); (NumInt 5);
            (NumInt 4); (NumInt 3); (NumInt 2); (NumInt 1)]); NumInt 3
       ] (Symbol "filter"));
  checkevalfail
    (apply_from_exprlist [(Lambda ("x", (Gt ((Symbol "x"), (NumInt 3))))); NumInt 3 ] (Symbol "filter"))

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