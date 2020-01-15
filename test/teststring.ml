open Minicaml.Types
open Util

module A = Alcotest

let sample_string= "\"hello world! nyasu \\t\""

let test_string () =
  check sample_string (EvtString("hello world! nyasu \t"))

let test_concat () =
  check ("\"a\" ++ \"b\"") (EvtString "ab");
  check ("concat \"a\" \"b\"") (EvtString "ab")

let test_suite = List.map quickcase [
    ("evaluate string", test_string);
    ("string concatenation", test_concat);
  ]