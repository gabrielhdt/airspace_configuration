# See https://github.com/ocaml/ocamlbuild/blob/master/examples/
DOCDIR = doc/ac.docdir
.PHONY: all clean byte native profile debug tags

OCB_FLAGS = -use-ocamlfind
OCB = ocamlbuild

all: native byte profile debug

clean:
	$(OCB) -clean

native:
	$(OCB) $(OCB_FLAGS) main.native astarmain.native

byte:
	$(OCB) $(OCB_FLAGS) main.byte astarmain.byte

profile:
	$(OCB) $(OCB_FLAGS) main.p.native

debug:
	$(OCB) $(OCB_FLAGS) main.d.byte

doc_html:
	$(OCB) $(OCB_FLAGS) $(DOCDIR)/index.html

doc_man:
	$(OCB) $(OCB_FLAGS) $(DOCDIR)/ac.man

doc_tex:
	$(OCB) $(OCB_FLAGS) $(DOCDIR)/ac.tex

doc_texinfo:
	$(OCB) $(OCB_FLAGS) $(DOCDIR)/ac.texi

doc_dot:
	$(OCB) $(OCB_FLAGS) $(DOCDIR)/ac.dot

tags:
	ctags src/*.ml src/*.mli Makefile
