## DONE - FIX PARSING ORDER IN APPLICATION
   ```ocaml
   let rec iter = fun n f d -> if n = 0 then d else iter (n - 1) (f) (f d) in let power = fun i n -> let itim = fun a -> a * i in iter (n) (itim) (1) in power 3 19
   ```
   AST EQUIVALENT
   ```ocaml
   (Letrec ("iter",
      (Lambda (["n"; "f"; "d"],
         (IfThenElse (Binop(Eq,(Symbol "n"), (NumInt 0))), (Symbol "d"),
            (Apply ((Symbol "iter"),
               [Binop(Sub,(Symbol "n"), (NumInt 1)));
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
                 [("itim", (Lambda (["a"], Binop(Mult,(Symbol "a"), (Symbol "i"))))))
                   ],
                 (Apply ((Symbol "iter"),
                    [(Apply ((Symbol "n"),
                        [(Apply ((Symbol "itim"), [(NumInt 1)]))]))
                      ]
                    ))
                 ))
              )))
           ],
         (Apply ((Symbol "power"), [(NumInt 3); (NumInt 2)]))))
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
         (IfThenElse (Binop(Eq,(Symbol "n"), (NumInt 0))), (Symbol "d"),
            (Apply ((Symbol "iter"),
               [Binop(Sub,(Symbol "n"), (NumInt 1))); (Symbol "f");
                 (Apply ((Symbol "f"), [(Symbol "d")]))]
               ))
            ))
         )),
      (Let (
         [("power",
           (Lambda (["i"; "n"],
              (Let (
                 [("itim", (Lambda (["a"], Binop(Mult,(Symbol "a"), (Symbol "i"))))))
                   ],
                 (Apply ((Symbol "iter"),
                    [(Symbol "n"); (Symbol "itim"); (NumInt 1)]))
                 ))
              )))
           ],
         (Apply ((Symbol "power"), [(NumInt 3); (NumInt 19)]))))
      ))
   ```