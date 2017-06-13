creating-words.pdf: creating-words.Rmd
	Rscript -e 'rmarkdown::render("$<", output_file = "$@")'
clean:
	rm -rf .cache/ figs/