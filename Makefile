

build:
	dune build --profile=release

install: build
	dune build @install
	dune install

clean:
	dune clean

uninstall:
	dune uninstall
