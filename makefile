all: vign2pdfhtml

vignettes: 
	cd vignettes;\
		Rscript -e 'library(knitr); knit("rmangal_basics.Rmd");'

vign2pdfhtml: vignettes
	cd vignettes;\
		pandoc rmangal_basics.md -o rmangal_basics.pdf;\
		pandoc rmangal_basics.md -o rmangal_basics.html;
