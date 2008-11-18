
EXAMPLE_SITE_HTML=main.html

EXAMPLE_SITE= $(EXAMPLE_SITE_HTML)

all: $(EXAMPLE_SITE)

.PHONY: nopdf

nopdf: $(EXAMPLE_SITE_HTML)

main.html: main.brtx
	m4 -D_todaydate="`date`" -D_output_format="HTML" main.brtx > main.brtx.tmp
	$(BRTX2HTMLDOC) -link-stylesheet ../$(CSS)  -i main.brtx.tmp -o $@.pp
	brtx -postpro -pp inlinehtml -i $@.pp -o $@

