CSS=brtxdoc.css
BRTX2HTMLDOC=brtx -html -doc -link-stylesheet $(CSS)


all: site/index.html

site/index.html: site/ src/index.brtx site/$(CSS)
	$(BRTX2HTMLDOC) -i src/index.brtx -o site/index.html

site/:
	mkdir site

site/$(CSS): site/ src/$(CSS)
	cp src/$(CSS) site/


.PHONY: clean

clean:
	rm -rf site
