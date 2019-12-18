## DONE - FIX PARSING ORDER IN APPLICATION
   ```ocaml
   let rec iter = fun n f d -> if n = 0 then d else iter (n - 1) (f) (f d) in let power = fun i n -> let itim = fun a -> a * i in iter (n) (itim) (1) in power 3 19
   ```
   AST EQUIVALENT
   ```ocaml
   (Letrec ("iter",
      (Lambda (["n"; "f"; "d"],
         (IfThenElse ((Eq ((Symbol "n"), (Integer 0))), (Symbol "d"),
            (Apply ((Symbol "iter"),
               [(Sub ((Symbol "n"), (Integer 1)));
                 (Apply ((Symbol "f"), [(Apply ((Symbol "f"), [(Symbol "d")]))]
                    ))
                 ]
               ))
            ))
         )),
      (Let (
         [("power",
           (Lambda (["i"; "n"],
              (Let (
                 [("itim", (Lambda (["a"], (Mult ((Symbol "a"), (Symbol "i"))))))
                   ],
                 (Apply ((Symbol "iter"),
                    [(Apply ((Symbol "n"),
                        [(Apply ((Symbol "itim"), [(Integer 1)]))]))
                      ]
                    ))
                 ))
              )))
           ],
         (Apply ((Symbol "power"), [(Integer 3); (Integer 2)]))))
      ))
   ```
   Should be equal to
   
   ```ocaml
   let rec iter = fun n f d -> if n = 0 then d else iter (n - 1) (f) (f d) in let
   power = fun i n -> let itim = fun a -> a * i in iter (n) (itim) (1) in power 3
   19
   ```
   AST EQUIVALENT
   ```ocaml
   (Letrec ("iter",
      (Lambda (["n"; "f"; "d"],
         (IfThenElse ((Eq ((Symbol "n"), (Integer 0))), (Symbol "d"),
            (Apply ((Symbol "iter"),
               [(Sub ((Symbol "n"), (Integer 1))); (Symbol "f");
                 (Apply ((Symbol "f"), [(Symbol "d")]))]
               ))
            ))
         )),
      (Let (
         [("power",
           (Lambda (["i"; "n"],
              (Let (
                 [("itim", (Lambda (["a"], (Mult ((Symbol "a"), (Symbol "i"))))))
                   ],
                 (Apply ((Symbol "iter"),
                    [(Symbol "n"); (Symbol "itim"); (Integer 1)]))
                 ))
              )))
           ],
         (Apply ((Symbol "power"), [(Integer 3); (Integer 19)]))))
      ))
   ```