all: creating-words-upload.tex creating-words.docx creating-words.pdf \
		 supplemental-materials/supplementary-figures.pdf \
		 supplemental-materials/inter-rater-reliability.docx \
		 supplemental-materials/selecting-words-to-learn-as-category-labels.docx \
		 supplemental-materials/instructions-given-to-participants.docx

creating-words-upload.tex: creating-words.tex
	sed 's!creating-words_files/figure-latex/!!g' creating-words.tex > $@
creating-words.tex: creating-words.pdf
creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "papaja::apa6_word")'
supplemental-materials/%.pdf: supplemental-materials/%.Rmd
	Rscript -e 'rmarkdown::render("$<")'
supplemental-materials/%.docx: supplemental-materials/%.Rmd
	Rscript -e 'rmarkdown::render("$<")'
clean:
	rm -rf *cache/ *files/ *.tex *.pdf *.docx
