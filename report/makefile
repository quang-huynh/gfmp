all: _book/resdoc.pdf

_book/resdoc.pdf: 01-introduction.Rmd 02-best-practices.Rmd 99-references.Rmd index.Rmd refs.bib csl/csas.csl _bookdown.yml 03-framework.Rmd 04-case-study.Rmd 05-discussion.Rmd 06-dlmtool-om.Rmd 07-msetool-sra.Rmd 08-default-slots.Rmd 09-mps.Rmd 11-om-rex.Rmd 10-mse-rex.Rmd 12-gfdlm.Rmd
	Rscript -e "bookdown::render_book('index.Rmd')"

continuous:
	while true; do make --silent; sleep 0.5; done

bibclean:
	bibtool -s --print.use.tab=Off --print.line.length=80 -i bib/refs.bib -o bib/refs.bib
