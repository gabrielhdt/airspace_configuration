LC = latexmk
FLAGS = -shell-escape -lualatex -synctex=1 -silent

.PHONY: %.pdf clean

all: prework.pdf article.pdf

%.pdf: %.tex %.bib
	$(LC) $(FLAGS) $<

clean:
	$(LC) -c
