.PHONY: build
build:
	dune build

test:
	ln -sf ../_build/install/default/bin/uniformize test/
	dune exec src/test/test.exe
	$(MAKE) -C test

.PHONY: install
install:
	dune install

.PHONY: clean
clean:
	 git clean -dfX
