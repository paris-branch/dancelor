.PHONY: build clean

build:
	dune build @install
	ln -sf _build/install/default/bin .

serve: build
	bin/imperator

clean:
	dune clean
	rm -f bin
