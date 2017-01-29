PACKAGES = -package ocaml-vdom -package xword -package xword.convert
SRCDIR = src/lib

.PHONY: all clean vdom css


vpath %.scss src/static
vpath %.ml src/lib

all : css vdom

css : crosspad.scss
	scss src/static/crosspad.scss > www/css/crosspad.css

crosspad :
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" src/lib/crosspad.js
	cp _build/src/lib/crosspad.js www/js

vdom :
	ocamlfind ocamlc $(PACKAGES) -I $(SRCDIR) -no-check-prims -linkpkg -o crosspad.exe \
	 	$(SRCDIR)/js_event.ml \
	 	$(SRCDIR)/crosspad_model.ml \
		$(SRCDIR)/crosspad_vdom.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js -o crosspad.js crosspad.exe
	mv crosspad.js www/js
	rm crosspad.exe

clean:
	ocamlbuild -clean
	rm src/lib/*.cm?
