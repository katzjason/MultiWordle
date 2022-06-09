.PHONY: test check

build:
	dune build

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

cloc:
	OCAMLRUNPARAM=b cloc --by-file --include-lang=OCaml .

clean:
	dune clean

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

zip:
	rm -f multiwordle.zip
	zip -r multiwordle.zip . -x@exclude.lst

docs:
	dune build @doc
