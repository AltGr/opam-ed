all: opam-ed opam-ed.1 opam-ed.install

PACKAGES = unix cmdliner opam-file-format

COMP ?= ocamlopt

opam-ed: src/opamEdMain.ml
	ocamlfind $(COMP) $(patsubst %,-package %,$(PACKAGES)) -linkpkg $^ -o $@

opam-ed.1: opam-ed
	./$< --help=groff >$@

opam-ed.install: opam-ed opam-ed.1
	./$< 'add bin ["opam-ed"]' 'add man ["opam-ed.1"]' <&- >$@

install: opam-ed opam-ed.install
	opam-installer opam-ed.install

tests: opam-ed
	tests/run.sh

clean:
	rm -f src/*.cm* src/*.o

distclean: clean
	rm -f opam-ed opam-ed.1 opam-ed.install
