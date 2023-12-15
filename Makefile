PROFILE=release

.PHONY: build install uninstall clean test doc format coverage

build:
	dune build @install --profile=$(PROFILE)

coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html
	bisect-ppx-report summary --per-file

install:
	dune install

uninstall:
	dune uninstall

clean:
	dune clean

test:
	dune runtest --profile=$(PROFILE)

check:
	dune build @check

# requires odoc
doc:
	dune build @doc --profile=$(PROFILE)

format:
	dune build @fmt --auto-promote
