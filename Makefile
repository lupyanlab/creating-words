DIR := submissions/$(NAME)

creating-words.pdf : creating-words.tex
	latexmk -pdf -pdflatex=/Library/TeX/texbin/pdflatex creating-words.tex
	latexmk -c
creating-words.tex : creating-words.utf.md methods.yaml
	pandoc creating-words.utf8.md pnas-sections.yaml methods.yaml --to latex --output creating-words.tex --template templates/pnas.tex --natbib
creating-words.docx : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', output_format = 'word_document')"
creating-words.utf.md : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', clean = FALSE, run_pandoc = FALSE)"
methods.yaml : methods.utf8.md
	pandoc methods.utf8.md --to latex --output methods.yaml --template templates/methods.yaml
methods.utf8.md : methods.Rmd
	Rscript -e "rmarkdown::render('methods.Rmd', clean = FALSE, run_pandoc = FALSE)"
submit : creating-words.pdf creating-words.docx
	mkdir -p $(DIR)
	mv creating-words.pdf $(DIR)/$(NAME).pdf
	mv creating-words.docx $(DIR)/$(NAME).docx
reset :
	rm -rf .cache/ figs/
clean :
	rm -f *.bbl *.xwm *.md *.tex
