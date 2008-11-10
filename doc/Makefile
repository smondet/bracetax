CSS=brtxdoc.css
BRTX2HTMLDOC=brtx -html -doc -link-stylesheet $(CSS)

WHOLE_SITE=site/ site/$(CSS)\
		   site/index.html \
		   site/brtx_app.html \
		   site/bracetax_syntax.html \
		   site/found_image.png \
		   site/tools.html

all: $(WHOLE_SITE)


site/:
	mkdir site

site/$(CSS): site/ src/$(CSS)
	cp src/$(CSS) site/

site/%.html:src/%.brtx
	$(BRTX2HTMLDOC) -i $< -o $@

site/found_image.png:
	cp `find /usr/share -name "*.png" | head -n 1` site/found_image.png

.PHONY: clean

clean:
	rm -rf site
