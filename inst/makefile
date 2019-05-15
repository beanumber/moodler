RMD := $(shell find . -type f -regex ".*\.Rmd")
TARGETS=$(RMD:%.Rmd=%.html)

%.html: %.Rmd
	@echo "$< -> $@"
	@Rscript -e "wordcountaddin::text_stats('$<')"
	@Rscript -e "rmarkdown::render('$<')"
	find . -name "*.Rmd" | wc
	find . -name "*.html" | wc

default: $(TARGETS)

unzip:
	find . -name '*.zip' -execdir unzip {} \;

clean:
	find . -name __MACOSX | xargs rm -rf
	find . -name *.html | xargs rm -rf
	find | rename "s/ /_/g"
	find | rename "s/ /_/g"
	find | rename "s/#/_/g"
	find | rename "s/\(/_/g"
	find | rename "s/\)/_/g"
	find . -name "*\.rmd" | rename "s/\.rmd/\.Rmd/g"
