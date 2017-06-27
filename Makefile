all: creating-words.pdf supporting-information.pdf
creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "papaja::apa6_word")'
supporting-information.pdf: supporting-information.Rmd
	Rscript -e 'rmarkdown::render("$<")'
clean:
	rm -rf *_cache/ *_files/ *.tex *.pdf *.docx
