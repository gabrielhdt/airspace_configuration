LC = latexmk
FLAGS = -shell-escape -lualatex -synctex=1 -silent

.PHONY: %.pdf clean

all: prework.pdf article.pdf

%.pdf: %.tex
	$(LC) $(FLAGS) $<

clean:
	$(LC) -c
