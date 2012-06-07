
all: presentation.pdf

presentation.pdf: $(wildcard *.tex) $(wildcard *.cls)
	latexmk -dvi- -pdf -pdflatex='lualatex %O -shell-escape %S' presentation.tex

.PHONY: all clean

clean: 
	latexmk -c
