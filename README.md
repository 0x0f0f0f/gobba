# minicaml

[Leggi qui in italiano](https://github.com/0x0f0f0f/minicaml/blob/master/README-it.md)

**minicaml** is a small, purely functional interpreted programming language with
a didactical purpose. I wrote **minicaml** for the **Programming 2** course at
the University of Pisa, taught by Professors Gianluigi Ferrari and Francesca
Levi. It is based on the teachers'
[minicaml](http://pages.di.unipi.it/levi/codice-18/evalFunEnvFull.ml), an
evaluation example to show how interpreters work. It is an interpreted subset of
Caml, with eager/lazy evaluation and only local (`let-in`) declaration statements. I
have added a simple parser and lexer made with menhir and ocamllex ([learn
more](https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)).
I have also added a simple REPL that show each reduction step that is done in
evaluating an expression. I'd like to implement a simple compiler and abstract
machine for this project.

**minicaml** only implements basic data types (integers and booleans) and will
never be a full programming language intended for real world usage. **minicaml's only
purpose is to help students get a grasp of how interpreters and programming
languages work**.

## Features

* Show the AST of each expression
* Easily choose between eager or lazy evaluation in your local definitions and
  functions by specifying lazyness using the `let lazy` statement.
* A simple AST optimizer
* Only boolean, list, unit and integer types (by now)
* Pretty color REPL showing every step made in evaluating a program
* Only local (optionally multiple) declaration statements
```ocaml
let x = 11 and y = 3 in x + y
```
* Recursive functions and closures
* ocamllex and menhir lexer and parser
* Extendable with ease

## Installation
I will release a binary file (no need to compile) in the near future. To
install, you need to have `opam` (OCaml's package manager) and a recent OCaml
distribution installed on your system.
[rlwrap](https://github.com/hanslub42/rlwrap) is suggested for a readline-like
(bash-like) keyboard interface.

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

Run `make run` to run a REPL. The REPL shows the AST equivalentof each submitted
expression, and each reduction step in the evaluation is shown. It also signals
syntactical and semantical errors.
