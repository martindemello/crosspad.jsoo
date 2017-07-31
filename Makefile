PACKAGES = -package ocaml-vdom \
					 -package xword \
					 -package xword.convert \
					 -package xword.crosspad-model
SRCDIR = src/lib

.PHONY: all clean vdom css


vpath %.scss src/static
vpath %.ml src/lib

all : css vdom

css : crosspad.scss
	scss src/static/crosspad.scss > www/css/crosspad.css

vdom :
	ocamlfind ocamlc $(PACKAGES) -I $(SRCDIR) -no-check-prims -linkpkg -o crosspad.exe \
	 	$(SRCDIR)/js_event.ml \
		$(SRCDIR)/crosspad_vdom.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js -o crosspad.js crosspad.exe
	mv crosspad.js www/js
	rm crosspad.exe

clean:
	ocamlbuild -clean
	rm src/lib/*.cm?
