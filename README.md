![https://travis-ci.org/0x0f0f0f/minicaml.svg?branch=master](https://travis-ci.org/0x0f0f0f/minicaml.svg?branch=master)
# minicaml 

**minicaml** is a small, purely functional interpreted programming language with
a didactical purpose. I wrote **minicaml** for the **Programming 2** course at
the University of Pisa, taught by Professors Gianluigi Ferrari and Francesca
Levi. It is based on the teachers'
[minicaml](http://pages.di.unipi.it/levi/codice-18/evalFunEnvFull.ml), an
evaluation example to show how interpreters work. It is an interpreted subset of
Caml, with eager and lazy evaluation. I have added a simple parser and lexer
made with menhir and ocamllex ([learn
more](https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)).
I have also added a simple REPL that show each reduction step that is done in
evaluating an expression. I'd like to implement a simple compiler and abstract
machine for this project.

**minicaml's purpose is to help students get a grasp of how interpreters and
programming languages work**.

* Recursive functions and closures
* ocamllex and menhir lexer and parser
* Extendable with (relative) ease

## Installation
To install, you need to have `opam` (OCaml's package manager) and a recent OCaml
distribution installed on your system.
You can install **minicaml** by running
```bash
opam install minicaml
```

[rlwrap](https://github.com/hanslub42/rlwrap) is suggested for a readline-like
(bash-like) keyboard interface.


### Manual installation
```bash
# clone the repository
git clone https://github.com/0x0f0f0f/minicaml
# cd into it
cd minicaml
# install dependencies
opam install dune menhir ANSITerminal cmdliner alcotest bisect_ppx
# compile
make
# test
make test
# run
make run
# rlwrap is suggested
rlwrap make run
# you can install minicaml with
make install
# run again
rlwrap minicaml
```

## Usage

The executable name is `minicaml`. If a file is specified as the first command
line argument, then it will be ran as a program. If you are running a program you may want to use the flag `-p` to print the results of the expressions that are evaluated. Otherwise, if a program is not specified a REPL session will
be opened. If the `minicaml` executable is ran with the flag `-v1`, it will show
the AST equivalent of each submitted expression, if ran with `-v2` it will also
show each reduction step in the evaluation.
Use the **experimental** `-j` flag to compile a program to Javascript, using the
[Ramda](https://ramdajs.com/) library as a "functional prelude", please note
that a lot of stuff is still broken.

Keep in mind that **minicaml** is purely functional and values
are immutable.

## Examples
Check the `examples/` directory for some example programs.

## Features

### Arithmetics with full scheme-like numeric tower
Integer division returns integers. Floating point numbers decimal part can be
omitted if it is 0. Floating point numbers can use the power syntax using `e`.
```ocaml
1 + 2 + 3 * (4 - 1) ;;
1 + 4.0 - 1. / 2.315 ;;
1.2e-3 ;;
true && false || (1 < 2) && (1 = 1) ;;
```

### Complex numbers
The `:+` and `:-` operators are used to create complex values, the floating point number
on the left is the real part and the one on the right is the imaginary part.
```ocaml
12. :+ 1.12;;
0. :- 1.12;;
```

### Strings and Lists
Here is how to concatenate strings
```ocaml
"hello " ^ "world"
```

`::` means is the classic `cons` operator, while `@` is used for list
concatenation as in OCaml
```
1 :: [2] @ [3]
```

To convert any value to a string you can use the `show` primitive.

### Declarations
Local declaration statements are purely functional and straightforward:
```ocaml
let x = 4 and y = 1 in x + y
```

Global declaration statements create new, purely functional environments in both
programs and the REPL. Omitting `in` is syntax-sugar, subsequent blocks will
be evaluated in the resulting new environment.
```ocaml
let a = 2 ;;
x + 3 ;;
```

### Functions and recursion
For parsing simplicity, only the OCaml anonymous function style of declaring
functions is supported. The keyword `fun` is interchangeable with `lambda`.  
```ocaml
(fun x -> x + 1) 1;;
let rec fib = fun n -> if n < 2 then n else (fib (n - 1)) + fib (n - 2)
```

### Toggle between safe and unsafe environments in code for I/O
You can choose to enable or disable unsafe primitives explicitely, inside an
expression or globally in the toplevel by using the `safe` and `unsafe`
keywords. An unsafe primitive is a primitive that has side effects, such as
direct memory access and I/O access. If you try running
```ocaml
print 2
```
Without enabling unsafe first, it yields
```
Error: Minicaml.Types.UnallowedUnsafe("print")
```

To enable unsafe primitives you can wrap an expression inside of an `unsafe`
statement, or just type `unsafe` alone in the toplevel to enable unsafe globally (not
recommended for a purely functional programming style!)

```
unsafe (print 2)
```
Will correctly print the number `2`.

### Printing
The unsafe primitives `print` and `print_endline` automatically call `show` on a
value. The difference between them is that `print_endline` automatically adds a
newline at the end of the line.

### A recommandation for writing safe/unsafe code
It is good practice to reduce the use of the `safe`/`unsafe` keywords as much as
possible, and to avoid using it inside of function bodies. This means keeping
your code as purely functional as you can.
```ocaml
(* Let's enable safeness globally *)
safe

(* BAD STYLE! *)
let bad_function = fun x ->
    unsafe (
        let mystring = "I am a bad function because i am using unsafe! Also: " ^ x in
        print mystring;
    );;

let bad = bad_function "hello!" ;;
(* The above will not error and may cause side effects *)

(* GOOD AND CONCISE STYLE! *)
let good_function = fun x -> print ("I am a good function! Also: " ^ x) ;;
let good = good_function "hello!" ;;
(* The above will error, because it causes side effects! Here's a good way of calling it*)
unsafe $ good_function "hello!" ;;
```

In the above example, you can clearly see how both the good and bad functions
return the same result, but the bad one is clearly less concise and complex
compared to the "good" one. You can mix `unsafe` and `safe` how you like, but
the suggestion is to move them as "outside" as you can, towards the toplevel
and not inside nested expressions

### Haskell-like dollar syntax
Too many parens?
```ocaml
f (g (h (i 1 2 3)))
```
Is equivalent to
```haskell
f $ g $ h $ i 1 2 3
```

### Function pipes (reverse composition)
You can redirect the result of a function to the first argument of another
function using the `>=>` operator.

### Dictionaries
```ocaml
let n = {"hola": 1, "mondo": 2} ;;
let m = insert "newkey" 123 n ;;
haskey "newkey" m (* => true *)
map (fun x -> x + 1) m
(* => {"newkey":124, "hola":2, "mondo":3} *)
```

