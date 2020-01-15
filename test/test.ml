open Minicaml.Types
module A = Alcotest

let test_stack_underflow () =
  A.check_raises "stack underflow" (Failure "Stack underflow") (fun () -> let _ =
                                                                            pop_stack EmptyStack in ())



let () = A.run "minicaml" [
    "stack", [
      A.test_case "stack underflow" `Quick test_stack_underflow;
      (*  A.test_case "stack overflow" `Quick test_stack_overflow *)
    ];
    "parser", Testparser.test_suite;
    "dictionaries", Testdict.test_suite;
    "lists", Testlist.test_suite;
    "strings", Teststring.test_suite;
    "purity", Testpurity.test_suite;
    "eval", Testeval.test_suite;
    (* "miscellanous programs", Testprogram.test_suite; *)
  ]