creating-words.pdf : creating-words.tex
	latexmk -pdf -pdflatex=/Library/TeX/texbin/pdflatex creating-words.tex > /dev/null
	latexmk -c
creating-words.docx : creating-words.Rmd
creating-words.tex : creating-words.utf.md
	pandoc creating-words.utf8.md pnas-sections.yaml --to latex --output creating-words.tex --template style/pnas.tex --natbib
creating-words.utf.md : creating-words.Rmd
	Rscript -e "rmarkdown::render('creating-words.Rmd', clean = FALSE, run_pandoc = FALSE)"
reset :
	rm -rf .cache/ figs/
clean :
	rm -f *.bbl *.xwm *.md *.tex
