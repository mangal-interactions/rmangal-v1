PDFS = rmangal_basics.pdf

all: $(PDFS)

%.pdf: %.Rnw
	$(R_HOME)/bin/Rscript -e "knitr::knit2pdf('$*.Rnw')"
