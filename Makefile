.PHONY: default build install uninstall run test clean

default: build

build:
	sed -i 's/\([^;]\)\( bisect_ppx)))\)/\1)));\2/' lib/dune
	dune build
	dune build @install

test:
	sed -i 's/)));\( bisect_ppx)))\)/\1/' lib/dune
	dune build
	dune build @install
	BISECT_ENABLE=yes MINICAML_EXAMPLES=$(realpath ./examples/) dune runtest -f
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