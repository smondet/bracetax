export CSS=brtxdoc.css
export BRTX2HTML=brtx -html 
export BRTX2HTMLDOC=brtx -html -link-css $(CSS)

TMPTEXDIR=/tmp/tmptex/
TMPTEXNAME=tmporary
TMPTEX=$(TMPTEXDIR)/$(TMPTEXNAME).tex

BRTX_STX_TITLE=Bracetax - Syntax

HTML_PAGES=site/ site/$(CSS)\
		site/index.html \
		site/brtx_app.html \
		site/bracetax_syntax.html \
		site/found_image.png \
		site/bracetax_syntax_raw.html \
		site/bracetax_syntax.brtx.html \
		site/git_log.html \
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
	$(BRTX2HTML) -doc -title "$(BRTX_STX_TITLE)" -i $< -o $@

site/bracetax_syntax_latex.pdf: src/bracetax_syntax.brtx site/found_image.png
	mkdir -p $(TMPTEXDIR) && \
	brtx -doc -title "$(BRTX_STX_TITLE)" -latex -i $< -o $(TMPTEX) && \
	cp site/found_image.png $(TMPTEXDIR) && cd $(TMPTEXDIR) && \
	pdflatex  $(TMPTEX) && pdflatex  $(TMPTEX) && \
	cd - && mv $(TMPTEXDIR)/$(TMPTEXNAME).pdf $@ && rm -rf $(TMPTEXDIR)

site/bracetax_syntax_latex_style.pdf: \
		src/bracetax_syntax.brtx site/found_image.png src/bracetaxdoc.sty
	mkdir -p $(TMPTEXDIR) && cp src/bracetaxdoc.sty  $(TMPTEXDIR) && \
	brtx -doc -title "$(BRTX_STX_TITLE)" -latex -i $< -o $(TMPTEX) \
	-use-package bracetaxdoc && \
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

site/git_log.html: site/
	git log | sed -e 's/\([{}#]\)/{\1}/g' | awk '\
		/^commit / {print "{p}{t|{b|Commit:} "$$2"}{br}"} \
		/^Author/  {print "{t|{b|Author:} " $$2 " " $$3 "}{br}"} \
		/^Date/    {print "{b|{t|" $$0 "}}{br}"} \
		/^    .+/    {if ($$0 !~ /.*git-svn-id.*/) { print "{t|{i|" $$0 "}}{br}" }} \
		/^   /    {if ($$0 ~ /.*git-svn-id.*/) print "{b|Git-SVN:}" $$2 "{br}"} \
		' | $(BRTX2HTMLDOC) -doc -title "Bracetax - git-log" -o $@


site/%.html:src/%.brtx
	$(BRTX2HTMLDOC) -doc -title "Bracetax - $*" -i $< -o $@

site/found_image.png:
	cp `find /usr/share -name "*.png" | head -n 1` site/found_image.png


clean:
	rm -rf site
