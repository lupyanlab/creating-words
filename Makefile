all : creating-words.pdf creating-words.docx
creating-words.pdf : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd')"
creating-words.docx : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', output_format = 'word_document')"
reset :
	rm -rf .cache/ figs/
