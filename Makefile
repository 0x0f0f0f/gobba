.PHONY: default build install uninstall run test clean

default: build

build:
	dune build

test:
	dune runtest -f

run:
	dune exec ./main.exe

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq