all : crosspad

crosspad : crosspad.ml
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" crosspad.js
	cp _build/crosspad.js www/js

clean:
	ocamlbuild -clean
