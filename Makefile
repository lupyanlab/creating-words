all: creating-words-upload.tex creating-words.docx creating-words.pdf supplemental-materials.pdf

creating-words-upload.tex: creating-words.tex
	sed 's!creating-words_files/figure-latex/!!g' creating-words.tex > $@
creating-words.tex: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<")'
creating-words.docx: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_format = "papaja::apa6_word")'
supplemental-materials.pdf: supplemental-materials.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file="$@")'
clean:
	rm -rf *cache/ *files/ *.tex *.pdf *.docx *.ent

example-chains.zip:
	zip -r example-chains example-chains

submit: all example-chains
	zip creating-words creating-words.pdf creating-words.docx creating-words-upload.tex supplemental-materials.pdf
	# Add figs without paths
	zip -j creating-words creating-words_files/figure-latex/*.pdf
	# Add example-chains as a compressed file
	zip creating-words example-chains.zip
