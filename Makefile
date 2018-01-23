# See https://github.com/ocaml/ocamlbuild/blob/master/examples/
DOCDIR = doc/docdir
.PHONY: all clean byte native profile debug

OCB = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native:
	$(OCB) main.native

byte:
	$(OCB) main.byte

profile:
	$(OCB) -tag profile main.native

debug:
	$(OCB) -tag debug main.byte

doc_html:
	$(OCB) $(DOCDIR)/index.html

doc_man:
	$(OCB) $(DOCDIR)/man

doc_tex:
	$(OCB) $(DOCDIR)/tex

doc_texinfo:
	$(OCB) $(DOCDIR)/texi

doc_dot:
	$(OCB) $(DOCDIR)/dot

tags:
	ctags src/*.ml src/*.mli Makefile
