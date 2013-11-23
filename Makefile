BIN=draw.cmo completion.cmo sources.cmo main.cmo
OCAMLC=ocamlfind ocamlc -package graphics,batteries,cmdliner -g
OBJECTS=draw.o draw-ml.o
%.o: %.c
	ocamlc -c $< -o $@ -g
dmlenu: $(OBJECTS) $(BIN:.cmo=.cmi) $(BIN)
	$(OCAMLC) -custom $(OBJECTS) -cclib -lX11 $(BIN) -o dmlenu -linkpkg -g

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

clean:
	rm -f *cm*
