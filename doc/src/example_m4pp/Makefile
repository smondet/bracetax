
EXAMPLE_SITE_HTML=main.html main.tex

EXAMPLE_SITE_VIM= Makefile.html main.brtx.html

EXAMPLE_SITE= $(EXAMPLE_SITE_HTML) $(EXAMPLE_SITE_VIM) main.pdf main.tex

all: $(EXAMPLE_SITE)

.PHONY: nopdf

nopdf: $(EXAMPLE_SITE_HTML) $(EXAMPLE_SITE_VIM)

#
# Generate the HTML page from source:
#                         m4              brtx -html       brtx -prospro    
#   source bracetax + M4 ---> pure bracetax  --->    HTML       --->     HTML
#
main.html: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="HTML" main.brtx > main.brtx.tmp
	$(BRTX2HTMLDOC) -link-stylesheet ../$(CSS)  -i main.brtx.tmp -o $@.pp
	brtx -postpro -pp inlinehtml -i $@.pp -o $@
	rm main.brtx.tmp  $@.pp


#
# Generate the HTML page from source:
#                         m4              brtx -html       brtx -prospro    
#   source bracetax + M4 ---> pure bracetax  --->    HTML       --->     HTML
#
main.tex: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="LATEX" main.brtx > main.brtx.tmp
	brtx -doc -latex  -i main.brtx.tmp -o main.tex.pp
	brtx -postpro -pp inlinelatex -i main.tex.pp -o main.tex
	rm main.brtx.tmp  main.tex.pp

#
# A PDF with pdflatex
#
main.pdf: main.tex
	pdflatex main && pdflatex main
	rm main.log main.aux main.out

#
# Using VIM to generate an HTML page from a source code
#
Makefile.html: Makefile
	vim -f -R $< \
		-c "sy on" \
		-c 'colorscheme darkblue' \
		-c TOhtml -c 'w! $@' -c 'qa!' 

#
# Using VIM to generate an HTML page from a source code
# (with the provided - incomplete - vim plugin
#
main.brtx.html: main.brtx
	vim -f -R $< \
		-c "sy on" \
		-c 'colorscheme darkblue' \
		-c 'source ../../../tools/bracetax_syntax.vim' \
		-c TOhtml -c 'w! $@' -c 'qa!' 

