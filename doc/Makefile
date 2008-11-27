export CSS=brtxdoc.css
export BRTX2HTML=brtx -html -doc 
export BRTX2HTMLDOC=brtx -html -doc -link-stylesheet $(CSS)

TMPTEXDIR=/tmp/tmptex/
TMPTEXNAME=tmporary
TMPTEX=$(TMPTEXDIR)/$(TMPTEXNAME).tex

HTML_PAGES=site/ site/$(CSS)\
		   site/index.html \
		   site/brtx_app.html \
		   site/bracetax_syntax.html \
		   site/found_image.png \
		   site/bracetax_syntax_raw.html \
		   site/bracetax_syntax.brtx.html \
		   site/tools.html

WHOLE_SITE=$(HTML_PAGES) \
		   site/bracetax_syntax_latex.pdf \
		   site/bracetax_syntax_latex_style.pdf 


.PHONY: clean nopdf example_m4pp_whole example_m4pp_nopdf

all: $(WHOLE_SITE) example_m4pp_whole

nopdf: $(HTML_PAGES) example_m4pp_nopdf

example_m4pp_whole: site/
	rm -rf site/example_m4pp 
	cp -r src/example_m4pp site/example_m4pp
	$(MAKE) -C site/example_m4pp

example_m4pp_nopdf: site/
	rm -rf site/example_m4pp 
	cp -r src/example_m4pp site/example_m4pp
	$(MAKE) -C site/example_m4pp nopdf



site/:
	mkdir site

site/$(CSS): site/ src/$(CSS)
	cp src/$(CSS) site/

site/bracetax_syntax_raw.html: src/bracetax_syntax.brtx site/found_image.png
	$(BRTX2HTML)  -i $< -o $@

site/bracetax_syntax_latex.pdf: src/bracetax_syntax.brtx site/found_image.png
	mkdir -p $(TMPTEXDIR) && \
	brtx -doc -latex -i $< -o $(TMPTEX) && \
	cp site/found_image.png $(TMPTEXDIR) && cd $(TMPTEXDIR) && \
	pdflatex  $(TMPTEX) && pdflatex  $(TMPTEX) && \
	cd - && mv $(TMPTEXDIR)/$(TMPTEXNAME).pdf $@ && rm -rf $(TMPTEXDIR)

site/bracetax_syntax_latex_style.pdf: \
		src/bracetax_syntax.brtx site/found_image.png src/bracetaxdoc.sty
	mkdir -p $(TMPTEXDIR) && cp src/bracetaxdoc.sty  $(TMPTEXDIR) && \
	brtx -doc -latex -i $< -o $(TMPTEX) -link-stylesheet bracetaxdoc && \
	cp site/found_image.png $(TMPTEXDIR) && cd $(TMPTEXDIR) && \
	pdflatex  $(TMPTEX) && pdflatex  $(TMPTEX) && \
	cd - && mv $(TMPTEXDIR)/$(TMPTEXNAME).pdf $@ && rm -rf $(TMPTEXDIR)


site/bracetax_syntax.brtx.html: src/bracetax_syntax.brtx site/found_image.png
	vim -f -R $< \
		-c "sy on" \
		-c 'colorscheme darkblue' \
		-c 'source ../tools/bracetax_syntax.vim' \
		-c TOhtml -c 'w! $@.tmp' -c 'qa!' && \
	sed -e 's/<title>.*<\/title>/<title>Bracetax Syntax - source<\/title>/' $@.tmp > $@
	rm -f $@.tmp


site/%.html:src/%.brtx
	$(BRTX2HTMLDOC) -i $< -o $@

site/found_image.png:
	cp `find /usr/share -name "*.png" | head -n 1` site/found_image.png


clean:
	rm -rf site
