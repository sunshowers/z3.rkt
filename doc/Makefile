
all: tfp.pdf

export TEXINPUTS=.:llncs/:
export BSTINPUTS=.:llncs/:

tfp.pdf: $(wildcard *.tex) citations.bib
	pdflatex tfp
	bibtex tfp
	pdflatex tfp
	pdflatex tfp

.PHONY: clean

clean: 
	-@echo "Cleaning generated files..."
	-@rm -f tfp.aux tfp.bbl tfp.blg
	-@rm -f tfp.log tfp.out tfp.pdf
