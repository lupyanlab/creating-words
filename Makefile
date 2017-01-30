creating-words.pdf : creating-words.tex
	latexmk -pdf creating-words
	latexmk -c
creating-words.tex : creating-words.knit.md
	pandoc --from markdown --to latex --output creating-words.tex --template templates/cogsci_template.tex creating-words.knit.md --bibliography telephone.bib --natbib
creating-words.knit.md : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', clean = FALSE, run_pandoc = FALSE)"
reset :
	rm -rf .cache/ figs/
