PACKAGES=-package ocaml-vdom -package xword -package xword.convert

all : crosspad

crosspad : crosspad.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" crosspad.js
	cp _build/crosspad.js www/js

vdom : crosspad_vdom.ml
	ocamlfind ocamlc $(PACKAGES) -no-check-prims -linkpkg -o crosspad.exe crosspad_model.ml js_event.ml crosspad_vdom.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js -o crosspad.js crosspad.exe
	mv crosspad.js www/js
	rm crosspad.exe

clean:
	ocamlbuild -clean
	rm *.cm?
