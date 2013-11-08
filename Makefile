BIN=completion.cmo main.cmo
OCAMLC=ocamlfind ocamlc -package graphics,batteries -g
dmlenu: $(BIN)
	$(OCAMLC) $(BIN) -o dmlenu -linkpkg

%.cmo: %.ml	
	$(OCAMLC) -c $< -o $@

clean:
	rm *cm*
