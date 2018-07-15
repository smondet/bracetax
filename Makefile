.PHONY: configure build clean default

default: build

configure:
	ocaml setup.ml -configure

build: configure
	ocaml setup.ml -build

clean:
	ocaml setup.ml -clean

VERSION=$(shell oasis query version)
NAME=bracetax-$(VERSION)

.PHONY: release
release:
	git tag -a -m $(VERSION) v$(VERSION)
	git archive --prefix=$(NAME)/ v$(VERSION) | gzip > $(NAME).tar.gz
	gpg -a -b $(NAME).tar.gz
