open Minicaml.Types
open Util

module A = Alcotest

let sample_dict = "{\"hello\":\"world\", \"apple\":314}"

let test_dict () =
  check sample_dict (EvtDict([(EvtString
  "hello", EvtString "world"); (EvtString "apple", EvtInt 314)]));
  check "{true:\"true\"}" (EvtDict([EvtBool true, EvtString "true"]));
  checkfail "{\"hello\":\"world\", \"hello\":\"world\", \"apple\":314}";
  checkfail "{(fun x -> x):1}"

let test_insert () =
  check ("insert 123 456 " ^ sample_dict)
  (EvtDict([(EvtInt 123, EvtInt 456);(EvtString
  "hello", EvtString "world"); (EvtString "apple", EvtInt 314)]));
  check ("insert \"hello\" 123 " ^ sample_dict)
  (EvtDict([(EvtString "hello", EvtInt 123); (EvtString "apple", EvtInt 314)]));
  checkfail ("insert \"doesntexist\" 123 " ^ sample_dict ^ " " ^ sample_dict)

let test_delete () =
  check ("delete \"hello\"" ^ sample_dict) (EvtDict([(EvtString "apple", EvtInt 314)]));
  checkfail ("delete \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict); 
  checkfail ("delete \"doesntexist\"" ^ sample_dict)
  
let test_haskey () =
  check ("haskey \"ciaone\" " ^ sample_dict) (EvtBool false);
  check ("haskey \"hello\" " ^ sample_dict) (EvtBool true);
  checkfail ("haskey \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_getkey () =
  check ("getkey \"hello\" " ^ sample_dict) (EvtString "world");
  checkfail ("getkey \"doesntexist\" " ^ sample_dict);
  checkfail ("getkey \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_map () =
  check "map (fun x -> x + 1) {\"a\":1,\"b\":2,\"c\":3,\"d\":4}" 
    (EvtDict
      [((EvtString "a"), (EvtInt 2)); ((EvtString "b"), (EvtInt 3));
      ((EvtString "c"), (EvtInt 4)); ((EvtString "d"), (EvtInt 5))]);
  checkfail "map \"fail\" \"fail\" \"fail\"" 

let test_foldl () =
  checkeval (Apply ((Symbol "foldl"),
   [(Lambda (["acc"; "x"], (Plus ((Symbol "acc"), (Symbol "x")))));
   (Integer 0);
   (Dict
    [((String "a"), (Integer 1)); ((String "b"), (Integer 2));
      ((String "c"), (Integer 3)); ((String "d"), (Integer 4))])
   ]
   )) (EvtInt 10);
   checkevalfail (Apply (Symbol "foldl", [String "fail"; String "fail"; String
   "Fail"; Integer 0]))

let test_filterkeys () =
  check ("filterkeys [\"apple\"] " ^ sample_dict)
  (EvtDict [(EvtString "apple", EvtInt 314)]);
  checkfail ("filterkeys \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_filter () =
  check "filter (fun x -> x > 3) {\"a\":1,\"b\":2,\"c\":3,\"d\":4}" 
  (EvtDict [((EvtString "d"), (EvtInt 4))])

let test_suite = List.map quickcase [
  ("evaluate dictionary", test_dict);
  ("insert in a dictionary", test_insert);
  ("delete from a dictionary", test_delete);
  ("haskey", test_haskey);
  ("getkey", test_getkey);
  ("map", test_map);
  ("foldl", test_foldl);
  ("filterkeys", test_filterkeys);
  ("filter", test_filter);
(*   ("no duplicate keys", test_duplicate) *)
]