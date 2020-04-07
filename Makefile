all:
	dune build @install

clean:
	@rm -r _build

examples:
	dune build @examples

.PHONY: all examples clean
