.PHONY: default build install uninstall run test clean

default: build

build:
	dune build
	dune build @install

test:
	BISECT_ENABLE=yes dune runtest -f
	bisect-ppx-report -html coverage/ -I _build/default _build/default/test/bisect*.out

run:
	dune exec ./bin/main.exe

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean
# Optionally, remove all files/folders ignored by git as defined
# in .gitignore (-X).
	git clean -dfXq