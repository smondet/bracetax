
EXAMPLE_SITE_HTML=main.html main.tex

EXAMPLE_SITE= $(EXAMPLE_SITE_HTML) main.pdf main.tex

all: $(EXAMPLE_SITE)

.PHONY: nopdf

nopdf: $(EXAMPLE_SITE_HTML)

main.html: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="HTML" main.brtx > main.brtx.tmp
	$(BRTX2HTMLDOC) -link-stylesheet ../$(CSS)  -i main.brtx.tmp -o $@.pp
	brtx -postpro -pp inlinehtml -i $@.pp -o $@
	rm main.brtx.tmp  $@.pp


main.tex: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="LATEX" main.brtx > main.brtx.tmp
	brtx -doc -latex  -i main.brtx.tmp -o main.tex.pp
	brtx -postpro -pp inlinelatex -i main.tex.pp -o main.tex
	rm main.brtx.tmp  main.tex.pp

main.pdf: main.tex
	pdflatex main && pdflatex main
	rm main.log main.aux main.out



