creating-words.pdf: creating-words.Rmd templates/preprint.tex
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf .cache/ figs/