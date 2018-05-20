LC = latexmk
FLAGS = -shell-escape -lualatex -synctex=1 -silent

.PHONY: %.pdf clean

all: article.pdf slideshow.pdf

%.pdf: %.tex article.bib
	$(LC) $(FLAGS) $<

clean:
	$(LC) -c
