
all: tfp.pdf

export TEXINPUTS=.:llncs/:
export BSTINPUTS=.:llncs/:

tfp.pdf: $(wildcard *.tex)
	pdflatex tfp
	bibtex tfp
	pdflatex tfp
	pdflatex tfp
