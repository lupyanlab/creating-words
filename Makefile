all: creating-words.pdf creating-words.docx supplemental-materials.pdf
creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "papaja::apa6_word")'
supplemental-materials.pdf: supplemental-materials.Rmd
	Rscript -e 'rmarkdown::render("$<")'
clean:
	rm -rf *_cache/ *_files/ *.tex *.pdf *.docx
