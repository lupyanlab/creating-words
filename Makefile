all: creating-words.pdf supporting-information.pdf
creating-words.pdf: creating-words.Rmd templates/nat-hum-beh.tex
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "word_document")'
supporting-information.pdf: supporting-information.Rmd templates/nat-hum-beh-si.tex
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf .cache*/ figs/ *.tex *.pdf *.docx
