.PHONY: build
build:
	dune build

test:
	dune exec src/test/test.exe

.PHONY: install
install:
	dune install

.PHONY: clean
clean:
	 git clean -dfX
