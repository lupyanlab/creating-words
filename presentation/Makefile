all: slides.pdf notes.pdf
slides.pdf: talk.Rmd analysis.R
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
notes.pdf: talk.Rmd analysis.R
	sed -e 's/<!--//' -e 's/-->//' talk.Rmd > notes.Rmd
	Rscript -e "rmarkdown::render('notes.Rmd', output_format = 'pdf_document', output_file = '$@')"
	rm notes.Rmd
img/plato.jpg:
	curl -o $@ https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Plato_Silanion_Musei_Capitolini_MC1377.jpg/320px-Plato_Silanion_Musei_Capitolini_MC1377.jpg?download
img/marcus.png:
	curl -o $@.tmp http://mperlman.org/images/marcus.png
	magick $@.tmp -resize 200x200 $@
	rm $@.tmp
img/pierce.png:
	curl -o $@.tmp http://psych.wisc.edu/cmsimages/pierce_edmiston2.jpg
	magick $@.tmp -resize 200x200 $@
	rm $@.tmp
img/chapman-sneeze.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Sneeze_Chapman/981616433.png
img/chapman-clap.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Clap_Chapman/83912d80a.png
clean:
	rm -rf talk_cache/ slides_files/
