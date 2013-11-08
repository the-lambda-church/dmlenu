BIN=completion.cmo sources.cmo main.cmo
OCAMLC=ocamlfind ocamlc -package graphics,batteries -g
dmlenu: $(BIN:.cmo=.cmi) $(BIN)
	$(OCAMLC) $(BIN) -o dmlenu -linkpkg

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

clean:
	rm *cm*
