MAKE_PID := $(shell echo $$PPID)
JOBS := $(shell ps T | sed -n 's/.*$(MAKE_PID).*$(MAKE).* \(-j\|--jobs\) *\([0-9][0-9]*\).*/\2/p')
ifeq ($(JOBS),)
JOBS := 1
endif


.PHONY: default build install uninstall run test clean

default: build

build:
	dune build -j $(JOBS) bin/main.exe
	dune build @install

doc: 
	dune build @doc -j $(JOBS) 

test:
	BISECT_ENABLE=yes dune build -j $(JOBS) 
	BISECT_ENABLE=yes dune build @install
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