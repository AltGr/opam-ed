all: opam-ed opam-ed.install

PACKAGES = unix cmdliner opam-file-format

opam-ed: src/opamEdMain.ml
	ocamlfind ocamlopt $(patsubst %,-package %,$(PACKAGES)) -linkpkg $< -o $@

opam-ed.install: opam-ed
	./opam-ed 'add bin ["opam-ed"]' <&- >$@

install: opam-ed opam-ed.install
	opam-installer opam-ed.install

clean:
	rm -f src/*.cm* src/*.o

distclean: clean
	rm -f opam-ed opam-ed.install
