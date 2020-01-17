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

test: export BISECT_ENABLE=yes
test:
	dune clean
	dune build -j $(JOBS)
	dune build @install
	GOBBA_EXAMPLES=$(realpath ./examples/) dune runtest -f
	bisect-ppx-report -html coverage/ -I _build/default _build/default/test/bisect*.out;
	# disabled coveralls upload because it is blocked 
	#@if [ -n "$$TRAVIS_JOB_ID" ]; then \
		echo "Uploading to coveralls..."; \
		bisect-ppx-report -html coverage/ -coveralls coverage.json -service-name travis -service-job-id $$TRAVIS_JOB_ID -I _build/default _build/default/test/bisect*.out; \
		curl -L -F json_file=@./coverage.json  https://coveralls.io/api/v1/jobs > /dev/null ; \
	else \
		bisect-ppx-report -html coverage/ -I _build/default _build/default/test/bisect*.out; \
	fi

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