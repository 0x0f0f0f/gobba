
# <p align="center"><img alt="gobba" src="assets/gobba.png" width = 45% /></p>

[![Build Status](https://travis-ci.org/0x0f0f0f/gobba.svg?branch=master)](https://travis-ci.org/0x0f0f0f/gobba) [![Coverage Status](https://coveralls.io/repos/github/0x0f0f0f/gobba/badge.svg?branch=)](https://coveralls.io/github/0x0f0f0f/gobba?branch=)

# gobba

**gobba** is a small, dynamically typed purely functional programming
language. It is heavily inspired by the OCaml, Haskell and Scheme languages.
**gobba** was initially written as a midterm project for  the **Programming
2** course at the University of Pisa, taught by Professors Gianluigi Ferrari and
Francesca Levi. Therefore, it is based on the teachers'
[gobba](http://pages.di.unipi.it/levi/codice-18/evalFunEnvFull.ml), a simple
evaluation example to show how interpreters work. Parsing and lexing are done
with menhir and ocamllex ([learn
more](https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html)).
The REPL can show each reduction step that is done in evaluating an expression.
An LLVM compiler is planned for the next future.

## Documentation
The internal documentation is available
[here](https://0x0f0f0f.github.io/gobba). Please note that the language at
the current status is in an experimental phase and therefore the specification
is not complete, and will be constantly updated. The documentation may not be up
to date with the latest features in the `master` branch.

## Installation
To install, you need to have `opam` (OCaml's package manager) and a recent OCaml
distribution installed on your system.
You can install **gobba** by running
```bash
opam install gobba
```

[rlwrap](https://github.com/hanslub42/rlwrap) is suggested for a readline-like
(bash-like) keyboard interface.


### Manual installation
```bash
# clone the repository
git clone https://github.com/0x0f0f0f/gobba
# cd into it
cd gobba
# install dependencies
opam install dune menhir ANSITerminal cmdliner alcotest bisect_ppx ocamline
# compile
make
# test
make test
# run
make run
# rlwrap is suggested
rlwrap make run
# you can install gobba with
make install
# run again
rlwrap gobba
```

## Usage

The executable name is `gobba`. If a file is specified as the first command
line argument, then it will be ran as a program. If you are running a program you may want to use the flag `-p` to print the results of the expressions that are evaluated. Otherwise, if a program is not specified a REPL session will
be opened.

Keep in mind that **gobba** is purely functional and values
are immutable by default!

### Command Line Options

* `--help[=FMT] (default=auto)`:
    Show this help in format FMT. The value FMT must be one of `auto`,
    `pager`, `groff` or `plain`. With `auto', the format is `pager` or
    `plain' whenever the TERM env var is `dumb' or undefined.

* `--internals`:
    To print or not the language's internal stack traces

* `-m MAXSTACKDEPTH, --maxstackdepth=MAXSTACKDEPTH (absent=10)`:
    The maximum level of nested expressions to print in a stack trace.

* `-p, --printexprs`:
    If set, print the result of expressions when evaluating a program
    from file

* `-v VERBOSITY, --verbose=VERBOSITY (absent=0)`:
    If 1, Print AST to stderr after expressions are entered in the
    REPL. If 2, also print reduction steps

* `--version`
    Show version information.

## Examples
Check the `examples/` directory for some example programs.

## Features

### Arithmetics with full scheme-like numeric tower
Integer division returns an integer if the modulo is 0, and returns a float
otherwise. Floating point numbers decimal part can be omitted if it is 0.
Floating point numbers can use the power syntax using `e`.
```ocaml
1 + 2 + 3 * (4 - 1) ;
1 + 4.0 - 1. / 2.315 ;
1.2e-3 ;
true && false || (1 < 2) && (1 = 1) ;
```


### Declarations
Local declaration statements are purely functional and straightforward:
```ocaml
let x = 4 and y = 1 in x + y
```

Global declaration statements create new, purely functional environments in both
programs and the REPL. Omitting `in` is syntax-sugar, subsequent blocks will
be evaluated in the resulting new environment.
```ocaml
let a = 2 ;
x + 3 ;
```

### Toplevel Directives
Toplevel directives can be used in both files and the REPL. Like in OCaml, they
start with a `#` symbol. Note that toplevel directives are not expressions and
they can only be used in a file (or REPL) top level, and cannot be used inside expressions.

`#include` loads a file at a position relative to the current directory (if in
the REPL) or the directory containing the current running file (in file mode).
The declarations in the included file will be included in the current toplevel environment:
```ocaml
#include "examples/fibonacci.mini"
```

`#module` loads a file like `#include` but the declarations in the included file
will be wrapped in a dictionary, that acts as a module:
```ocaml
#module "examples/fibonacci.mini"
(* Declarations will be available in module *) Fibonacci
```

`#verbosity n` sets verbosity level to `n`. There are "unit" directives:
`#dumpenv ()` and `#dumppurityenv ()` dump the current environments. `#pure ()`,
`#impure ()` and `#uncertain ()` set the globally allowed purity level.

### Complex numbers
The `:+` and `:-` operators are used to create complex values, the floating point number
on the left is the real part and the one on the right is the imaginary part.
```ocaml
12. :+ 1.12;
0. :- 1.12;
```

### Character literals.
The same as all the other languages: Single characters enclosed in `'` are character literals,
such as `'a'` or `'\n'`. UTF-8 support is planned for a future release.

### Strings and Lists
Here is how to concatenate strings
```haskell
"hello " ++ "world"
(* It is the same as *)
String:concat "hello " "world"
```

To convert any value to a string you can use the `show` primitive.

`::` means is the classic `cons` operator, while `++` is used for list and string concatenation
```haskell
1 :: [2] ++ [3]
```

To access nth value of a list, the `@` (at) operator is used. Lists are indexed from 0.

```ocaml
[1, 2, 3, 4] @ 0 (* => 1 *)
[1, 2, 3, 4] @ 2 (* => 3 *)
```


### Functions and recursion
For parsing simplicity, only the OCaml anonymous function style of declaring
functions is supported. The keyword `fun` is interchangeable with `lambda`.  
```ocaml
(fun x -> x + 1) 1;
let fib = fun n -> if n < 2 then n else (fib (n - 1)) + fib (n - 2)
```

Functions are abstracted into a single parameter chain of functions, and they
can be partially applied:

```ocaml
(fun x y z -> x + y + z) = (fun x -> fun y -> fun z -> x + y + z) ;
(* result: true - bool - This is true!! *)

let f = (fun x y z -> x + y + z) in f 1 2 3 ;
(* result: 6 - int - Function application *)

let f = (fun x y z -> x + y + z) in f 1 2 ;
(* result: (fun z -> ... ) - fun - Partial application *)
```


### Dictionaries and modules.
Dictionary (object) values are similar to Javascript objects. The difference
from javascript is that the keys of an existing dictionary are treated as
symbols, and values can be lazy.

You may have noticed that dictionary fields are syntactically similar to the
assignments in `let` statements. This is because there is a strict approach
towards simplicity in the parsing logic and language syntax. A difference from
`let` statements, is that values in dictionaries can only access the
lexical scope **outside** of the dictionary.


```ocaml
let n = {hola = 1, lazy mondo = 2, somefunc = fun x -> x + 1 } ;
let m = Dict:insert "newkey" 123 n ;
m = {newkey = 123, hola = 1, mondo = 2, somefunc = fun x -> x + 1 } (* => true *)
Dict:haskey "newkey" m (* => true *)
map (fun x -> x + 1) m
(* => {newkey = 124, hola = 2, mondo = 3} *)
```

An element of a dictionary can be accessed using the `:` infix operator.
```ocaml
m:hola (* returns 1 *)
```

### Primitives and printing
The impure primitives `IO:print` and `IO:print_endline` automatically call `show` on a
value. The difference between them is that `IO:print_endline` automatically adds a
newline at the end of the line.



### Haskell-like dollar syntax
Too many parens?
```ocaml
f (g (h (i 1 2 3)))
```
Is equivalent to
```haskell
f $ g $ h $ i 1 2 3
```

### Toggle between pure and impure environments in code for I/O
You can choose to enable or disable impure primitives explicitely, inside an
expression by wrapping it into the `pure` and `impure` statements. They must be
followed by an expression. An expression contained in an `impure` statement is a
computation that calls primitives that have side effects, such as direct memory
access or I/O access.

It is good practice to reduce the use of the `pure/impure` keywords as much as
possible, and to avoid using it inside of function bodies. This means keeping
your code as purely functional as you can.
```ocaml
let bad_function = fun x ->
    impure (let mystring =
        "I am a bad impure function! Also: " ++ x in
        IO:print_endline mystring );

let good_function = fun x ->
    IO:print_endline ("I am a good function! Also: " ++ x) ;

bad_function "hello!" ;
(* The above statement is causing side effects and will error *)

good_function "hello! I should error" ;
(* The above will error, because it is trying to execute
an impure computation in a pure environment
Here's a good way of calling it *)
impure $ good_function "hello!" ;

(* You can specify that you DO NOT want to compute impure
expressions by using the pure statement *)
pure $ good_function "henlo world! I should error" ;
(* The above will error because
it contains an impure computation*)
pure $ bad_function "ciao mondo! I should error" ;
(* The above will error because a pure contest
does not allow nesting an impure contest inside *)
```

A good way of structuring your code is keeping `pure/impure` statements as
external from expressions as you can (towards the top level). By default, the
interpreter is in a `uncertain` state, it means that it will allow the execution
of `impure` statements

### Function pipes (reverse composition) and composition
You can redirect the result of a function to the first argument of another
function using the `>=>` operator.
```ocaml
let sum_and_add_one = (fun x y -> x + y) >=> (fun z -> z + 1) ;
sum_and_add_one 2 3
(* Will output 6, because 2 + 3 is piped into z + 1*)
```
Yields the same result as normal composition:
```ocaml
let my_sum = (fun x y -> x + y) ;
let add_one = (fun z -> z + 1) ;
(add_one <=< my_sum) 2 3 = add_one (my_sum 2 3) ;
(* The operator <=< means compose *)
(add_one <=< my_sum) = (my_sum >=> add_one) ;
(* This is also true! *)
```

### Sequencing (>>) operator
TODO