TEXT = index

all: $(TEXT).pdf

%.pdf: %.Rmd
	Rscript -e "bookdown::render_book('index.Rmd')"

continuous:
	while true; do make --silent; sleep 5; done
