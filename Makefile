slides.pdf: talk.Rmd R/*.R
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"