
# <p align="center"><img alt="gobba" src="assets/gobba.png" width = 25% /></p>

[![Build Status](https://travis-ci.org/0x0f0f0f/gobba.svg?branch=master)](https://travis-ci.org/0x0f0f0f/gobba) [![Coverage Status](https://coveralls.io/repos/github/0x0f0f0f/gobba/badge.svg?branch=)](https://coveralls.io/github/0x0f0f0f/gobba?branch=)

# gobba

**gobba** is a dynamically typed and purely functional interpreted
programming language, heavily inspired from the OCaml,
Haskell and Scheme languages. It is based on Professors Gianluigi Ferrari and Francesca Levi's
[minicaml](http://pages.di.unipi.it/levi/codice-18/evalFunEnvFull.ml) interpreter example. The goal for gobba is to be a practical language
with built in support for scientific computing, solving some of the problems
that exist in other dynamically typed interpreted languages like python and
Javascript. It features static (lexical scoping), a simple but
effective module system, eager and lazy evaluation and an interactive didactical
REPL that shows completion hints and can print insights about each recursive evaluation step.

## Features
Check out the [Basics Chapter](https://0x0f0f0f.github.io/gobba-book/basics) in the **Gobba Handbook**.

## Documentation

You can read the Gobba documentation in the [Gobba Programming Language Handbook](https://0x0f0f0f.github.io/gobba-book)

The internal documentation is available
[here](https://0x0f0f0f.github.io/gobba). Please note that the language at the
current status is in an experimental phase and therefore the specification is
not complete, and will be constantly updated. The documentation may not be up to
date with the latest features in the `master` branch.

## Installation
To install, you need to have `opam` (OCaml's package manager) and a recent OCaml
distribution installed on your system.
You can install **gobba** by running
```bash
opam install gobba
```

If you want to compile gobba manually, read the [installation chapter in the gobba Handbook](https://0x0f0f0f.github.io/gobba-book/install.html)

## Examples
Check the `examples/` directory for some example programs.

