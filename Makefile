creating-words.docx : creating-words.Rmd
		Rscript -e "rmarkdown::render('creating-words.Rmd')"
reset :
	rm -rf .cache/ figs/
