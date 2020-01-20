open Util
open Gobba
open Types

module A = Alcotest

let sample_list = "[\"hello\", \"world\", \"apple\", 314]"
let sample_list_evt = (EvtList([EvtString "hello"; EvtString "world"; EvtString "apple"; EvtInt 314]))

let test_list () =
  check sample_list sample_list_evt

let test_head () =
  check ("List:head " ^ sample_list) (EvtString "hello");
  checkfail "List:head []";
  checkfail "List:head [] 2";
  checkfail "List:head 2 3"

let test_tail () =
  check ("List:tail " ^ sample_list) (EvtList([EvtString "world"; EvtString "apple"; EvtInt 314]));
  checkfail "List:tail []";
  checkfail "List:tail [] 2";
  checkfail "List:tail 2 3"

let test_cons () =
  check "\"hello\"::[]"(EvtList [EvtString "hello"]);
  check "\"hello\"::[\"world\"]"  (EvtList [EvtString "hello"; EvtString "world"]);
  checkfail "4::2"

let test_map () =
  check "List:map (fun x -> x + 1) [1, 2, 3, 4]" (EvtList [EvtInt 2; EvtInt 3; EvtInt 4; EvtInt 5]);
  checkfail "List:map \"fail\" \"fail\" \"fail\"";
  checkfail "List:map (fun x -> x + 1) 3";
  checkfail "List:map 3 [1, 2]"

let test_fold () =
  check "List:foldl (fun acc x -> acc + x) 0 [1,2,3,4]"
    (EvtInt 10);
  check "List:foldl (fun x y -> x - y) 10 [1,2,3]" (EvtInt 4);
  check "List:foldr (fun x y -> x - y) 10 [1,2,3]" (EvtInt (-8));
  checkfail "List:foldl \"fail\" \"fail\" \"fail\" 0";
  checkfail "List:foldl (fun x -> 1 + x) 0 \"x\""

let test_filter () =
  check "List:filter (fun x -> x > 3) [1,2,3,4,5,4,3,2,1]" (EvtList [(EvtInt 4); (EvtInt 5); (EvtInt 4)]);
  check "List:filter (fun x -> x < 0) [1,2,3,4,5,4,3,2,1]" (EvtList []);
  checkfail "List:filter (fun x -> x > 3) [1,2,3,4,5,4,3,2,1] 3";
  checkfail "List:filter 3 [1,2,3]";
  checkfail "List:filter (fun x -> x > 3) 3"

let test_concat () =
  check ("[1] ++ [2]") (EvtList [EvtInt 1; EvtInt 2])


let test_suite = List.map quickcase [
    ("evaluate list", test_list);
    ("head", test_head);
    ("tail", test_tail);
    ("cons", test_cons);
    ("concat", test_concat);
    ("map", test_map);
    ("fold", test_fold);
    ("filter", test_filter);
  ]