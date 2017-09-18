slides.pdf: talk.Rmd analysis.R img/plato.jpg img/chapman-sneeze.png img/chapman-clap.png
	Rscript -e "rmarkdown::render('$<', output_file = '$@')"
img/plato.jpg:
	curl -o $@ https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Plato_Silanion_Musei_Capitolini_MC1377.jpg/320px-Plato_Silanion_Musei_Capitolini_MC1377.jpg?download
img/chapman-sneeze.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Sneeze_Chapman/981616433.png
img/chapman-clap.png:
	curl -o $@ https://cdn.theatlantic.com/assets/media/img/posts/2015/11/Clap_Chapman/83912d80a.png
clean:
	rm -rf *_cache/ *_files/
