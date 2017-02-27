NAME ?= pnas
DIR := submissions/$(NAME)

all : creating-words.pdf supporting-information.pdf
creating-words.pdf : creating-words.tex
	latexmk -pdf -pdflatex=/Library/TeX/texbin/pdflatex creating-words.tex
creating-words.tex : creating-words.utf.md methods.yaml
	pandoc creating-words.utf8.md pnas-sections.yaml methods.yaml --to latex --output creating-words.tex --template templates/pnas.tex --natbib
creating-words.docx : creating-words-with-methods.Rmd
	Rscript -e "rmarkdown::render('creating-words-with-methods.Rmd', output_format = 'word_document', output_file = 'creating-words.docx')"
creating-words-with-methods.Rmd : creating-words.Rmd methods.utf8.md
	sed '1,5d' methods.utf8.md > methods.md
	sed 's/References/Methods/' creating-words.Rmd > creating-words-with-methods.Rmd
	sed -i .bk '/# Methods/r methods.md' creating-words-with-methods.Rmd
	echo "\n# References {-}" >> creating-words-with-methods.Rmd
	rm creating-words-with-methods.Rmd.bk
creating-words.utf.md : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', clean = FALSE, run_pandoc = FALSE)"
methods.yaml : methods.utf8.md
	pandoc methods.utf8.md --to latex --output methods.yaml --template templates/methods.yaml
methods.utf8.md : methods.Rmd
	Rscript -e "rmarkdown::render('methods.Rmd', clean = FALSE, run_pandoc = FALSE)"
supporting-information.pdf : supporting-information.Rmd
	Rscript -e "rmarkdown::render('supporting-information.Rmd')"
supporting-information.docx : supporting-information.Rmd
	Rscript -e "rmarkdown::render('supporting-information.Rmd', output_format = 'word_document')"
submit : creating-words.pdf creating-words.docx supporting-information.pdf supporting-information.docx
	mkdir -p $(DIR)
	mv creating-words.pdf $(DIR)/$(NAME).pdf
	mv creating-words.docx $(DIR)/$(NAME).docx
	mv supporting-information.pdf $(DIR)/
	mv supporting-information.docx $(DIR)/
reset :
	rm -rf .cache/ figs/
clean :
	latexmk -c || true
	rm -f *.bbl *.xwm *.md *.tex *.aux *.fdb_latexmk *.fls *.log *.out *-with-methods*
