all: _book/resdoc.pdf

_book/resdoc.pdf: 01-introduction.Rmd 02-best-practices.Rmd 03-summary-shortraker.Rmd 04-summary-rex.Rmd 05-discussion.Rmd 06-mp-descriptions.Rmd 07-dlmtool-om.Rmd 09-default-slots.Rmd 10-om-rex.Rmd 11-mse-results-rex.Rmd 99-references.Rmd index.Rmd bib/refs.bib csl/csas.csl
	Rscript -e "bookdown::render_book('index.Rmd')"

continuous:
	while true; do make --silent; sleep 5; done
