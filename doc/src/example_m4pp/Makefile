
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
HEVEA_OR_NOT= hevea || echo NoHeveaFound
main.html: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="HTML" main.brtx > main.brtx.tmp
	$(BRTX2HTML) -doc -title "Bracetax Example" -link-css ../$(CSS) \
	    -i main.brtx.tmp -o $@
	rm main.brtx.tmp


#
# Generate the HTML page from source:
#                         m4              brtx -html       brtx -prospro    
#   source bracetax + M4 ---> pure bracetax  --->    HTML       --->     HTML
#
main.tex: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="LATEX" main.brtx > main.brtx.tmp
	brtx -doc -title "Self Documenting Example" -latex  -i main.brtx.tmp -o main.tex
	rm main.brtx.tmp 

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
		-c TOhtml -c 'w! $@.tmp' -c 'qa!' && \
	sed -e 's/<title>.*<\/title>/<title>Makefile<\/title>/' \
		$@.tmp > $@
	rm -f $@.tmp

#
# Using VIM to generate an HTML page from a source code
# (with the provided - incomplete - vim plugin
#
main.brtx.html: main.brtx
	vim -f -R $< \
		-c "sy on" \
		-c 'colorscheme darkblue' \
		-c 'source ../../../tools/bracetax_syntax.vim' \
		-c TOhtml -c 'w! $@.tmp' -c 'qa!' && \
	sed -e 's/<title>.*<\/title>/<title>Example - source<\/title>/' \
		$@.tmp > $@ 
	rm -f $@.tmp

