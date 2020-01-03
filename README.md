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

## Features

* Show the AST of each expression
* Easily choose between eager or lazy evaluation in your local definitions and
  functions by specifying lazyness using the `let lazy` statement.
* Boolean, list, unit, integer, dictionary, function and string types.
* A simple AST optimizer
* Pretty color REPL showing every step made in evaluating a program
* Local (optionally multiple) declaration statements
```ocaml
let x = 11 and y = 3 in x + y
```
* Global declaration statements create new, purely functional environments in
  both programs and the REPL.
```ocaml
let x = 11 and y = 3;;
x + y
(* result is 14 *)
```
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

Keep in mind that **minicaml** is purely functional and values
are immutable.

## Examples
Check the `examples/` directory for some example programs.

### Arithmetics
```ocaml
1 + 2 + 3 * (4 - 1) ;;
true && false || (1 < 2) && (1 = 1) ;;
```

### Strings and Lists
```ocaml
"hello " ^ "world"
1 :: [2] @ [3]
```

### Declarations
```ocaml
let x = 4 and y = 1 in x + y
(* Omitting `in` is syntax-sugar, subsequent blocks will
be evaluated in the resulting new environment *)
let a = 2
x + 3
```

### Functions and recursion
```ocaml
(fun x -> x + 1) 1;;
let rec fib = fun n -> if n < 2 then n else (fib (n - 1)) + fib (n - 2)
```

### Dictionaries
```ocaml
let n = {"hola": 1, "mondo": 2} ;;
let m = insert "newkey" 123 n ;;
haskey "newkey" m (* => true *)
map (fun x -> x + 1) m
(* => {"newkey":124, "hola":2, "mondo":3} *)
```