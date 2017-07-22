all: creating-words.pdf creating-words.docx
creating-words-upload.tex: creating-words.tex
	sed 's!creating-words_files/figure-latex/!!g' creating-words.tex > $@
creating-words.tex: creating-words.pdf
creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "papaja::apa6_word")'
clean:
	rm -rf *_cache/ *_files/ *.tex *.pdf *.docx
