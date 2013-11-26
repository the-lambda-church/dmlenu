-include Makefile.config

BIN=draw.cmo completion.cmo sources.cmo dmlenu.cmo main.cmo
OCAMLC=ocamlfind ocamlc -package batteries,cmdliner -g
OBJECTS=draw.o draw-ml.o
%.o: %.c
	ocamlc -c $< -o $@ -g

dmlenu: $(OBJECTS) $(BIN:.cmo=.cmi) $(BIN)
	$(OCAMLC) -custom $(OBJECTS) -cclib -lX11 $(BIN) -o dmlenu -linkpkg -g

%.cmo: %.ml
	$(OCAMLC) -c $< -o $@

%.cmi: %.mli
	$(OCAMLC) -c $< -o $@

CONFIGURE = Makefile.config
$(CONFIGURE):
	@echo "Please run ./configure"
	@false

install: $(CONFIGURE) dmlenu
	install dmlenu $(DEST_DIR)/dmlenu
	install dmlenu_run $(DEST_DIR)/dmlenu_run

uninstall: $(CONFIGURE)
	rm $(DEST_DIR)/dmlenu
	rm $(DEST_DIR)/dmlenu_run

clean:
	rm -f Makefile.config
	rm -f *cm*
	rm -f *.o
	rm -f dmlenu

.PHONY: clean install uninstall
