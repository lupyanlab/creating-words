slides.pdf: talk.Rmd analysis.R
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
img/gary.jpg:
	curl -o $@ http://sapir.psych.wisc.edu/wp-content/uploads/2016/05/lupyan_2015_sm-253x300.jpg
img/marcus.png:
	curl -o $@.tmp http://mperlman.org/images/marcus.png
	magick $@.tmp -resize 200x200 $@
	rm $@.tmp
img/pierce.png:
	curl -o $@.tmp http://psych.wisc.edu/cmsimages/pierce_edmiston2.jpg
	magick $@.tmp -resize 200x200 $@
	rm $@.tmp
img/plato.jpg:
	curl -o $@ https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Plato_Silanion_Musei_Capitolini_MC1377.jpg/320px-Plato_Silanion_Musei_Capitolini_MC1377.jpg?download
img/chapman-sneeze.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Sneeze_Chapman/981616433.png
img/chapman-clap.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Clap_Chapman/83912d80a.png
