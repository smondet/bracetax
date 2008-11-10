CSS=brtxdoc.css
BRTX2HTMLDOC=brtx -html -doc -link-stylesheet $(CSS)

WHOLE_SITE=site/ site/$(CSS)\
		   site/index.html \
		   site/brtx_app.html \
		   site/bracetax_syntax.html \
		   site/tools.html

all: $(WHOLE_SITE)


site/:
	mkdir site

site/$(CSS): site/ src/$(CSS)
	cp src/$(CSS) site/

site/%.html:src/%.brtx
	$(BRTX2HTMLDOC) -i $< -o $@

.PHONY: clean

clean:
	rm -rf site
