open Gobba.Types
open Util

module A = Alcotest

let sample_dict = "{hello = \"world\"; apple = 314}"

let test_dict () =
  check sample_dict (EvtDict([("hello", EvtString "world"); ( "apple", EvtInt 314)]));
  check "{trueval =\"true\"}" (EvtDict(["trueval", EvtString "true"]));
  checkfail "{hello =\"world\", hello=\"world\", apple=314}";
  checkfail "{(fun x -> x) =1}"

let test_insert () =
  check ("Dict:insert \"abc\" 456 " ^ sample_dict)
    (EvtDict([("abc", EvtInt 456);("hello", EvtString "world"); ("apple", EvtInt 314)]));
  check ("Dict:insert \"hello\" 123 " ^ sample_dict)
    (EvtDict([("hello", EvtInt 123); ("apple", EvtInt 314)]));
  checkfail ("insert \"doesntexist\" 123 " ^ sample_dict ^ " " ^ sample_dict)

let test_remove () =
  check ("Dict:remove \"hello\"" ^ sample_dict) (EvtDict([("apple", EvtInt 314)]));
  checkfail ("Dict:remove \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict);
  checkfail ("Dict:remove \"doesntexist\"" ^ sample_dict)

let test_haskey () =
  check ("Dict:haskey \"ciaone\" " ^ sample_dict) (EvtBool false);
  check ("Dict:haskey \"hello\" " ^ sample_dict) (EvtBool true);
  checkfail ("Dict:haskey \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_getkey () =
  check (sample_dict ^ ":hello") (EvtString "world");
  check ("( " ^ sample_dict ^ " ):hello") (EvtString "world");
  checkfail ("Dict:getkey \"doesntexist\" " ^ sample_dict);
  checkfail ("Dict:getkey \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_map () =
  check "map (fun x -> x + 1) {a=1;b=2;c=3;d=4}"
    (EvtDict
       [(("a"), (EvtInt 2)); (("b"), (EvtInt 3));
        (("c"), (EvtInt 4)); (("d"), (EvtInt 5))]);
  checkfail "map \"fail\" \"fail\" \"fail\""

let test_fold () =
  check "foldl (fun acc x -> x + acc) 10 {a= 1; b= 2; c= 3; d= 4}" (EvtInt 20);
  check "foldl (fun x y -> x - y) 10 {a=1; b=2; c=3}" (EvtInt 4);
  check "foldr (fun x y -> x - y) 10 {a=1; b=2; c=3}" (EvtInt (-8));
  checkfail "foldl \"fail\" \"fail\"  \"fail\" 0"

let test_filterkeys () =
  check ("Dict:filterkeys [\"apple\"] " ^ sample_dict)
    (EvtDict [("apple", EvtInt 314)]);
  checkfail ("filterkeys \"doesntexist\" " ^ sample_dict ^ " " ^ sample_dict)

let test_filter () =
  check "filter (fun x -> x > 3) {a=1;b=2;c=3;d=4}"
    (EvtDict [(("d"), (EvtInt 4))])

let test_suite = List.map quickcase [
    ("evaluate dictionary", test_dict);
    ("insert in a dictionary", test_insert);
    ("remove from a dictionary", test_remove);
    ("haskey", test_haskey);
    ("getkey", test_getkey);
    ("map", test_map);
    ("fold", test_fold);
    ("filterkeys", test_filterkeys);
    ("filter", test_filter);
  ]