open Minicaml.Types
module A = Alcotest

let test_stack_underflow () =
A.check_raises "stack underflow" (Failure "Stack underflow") (fun () -> let _ =
pop_stack EmptyStack in ())

(* let test_stack_overflow () =
  A.check_raises "stack overflow" (Failure "Stack overflow")
    (fun () -> let s = ref EmptyStack in
    while true do
      s := push_stack !s (Integer 0)
    done) *)

(* let test_optimizer_1 = A.(check expr) *)
(*
let optimizer_tests = [
  A.test_case "optimizer 1" `Quick test_optimizer_1
] *)

let () = A.run "minicaml" [
  "stack", [
    A.test_case "stack underflow" `Quick test_stack_underflow;
   (*  A.test_case "stack overflow" `Quick test_stack_overflow *)
  ];
  "parser", Testparser.test_suite;
  "dictionaries", Testdict.test_suite
]