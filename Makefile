creating-words.pdf: creating-words.Rmd templates/nat-hum-beh.tex
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "word_document")'
clean:
	rm -rf .cache/ figs/ *.pdf *.docx
