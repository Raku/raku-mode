CASK = cask
EMACS = emacs
EMACSFLAGS = --eval "(add-to-list 'load-path \".\")"
TESTFLAGS =

export EMACS

PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

SRCS := $(wildcard *.el)
OBJS = $(SRCS:.el=.elc)

.PHONY: compile test clean

compile: $(OBJS)

clean:
	rm -rf $(OBJS) dist raku-mode-pkg.el

test: $(PKGDIR)
	$(CASK) exec ert-runner $(TESTFLAGS)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACS) -Q --batch $(EMACSFLAGS) -f batch-byte-compile $<

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

dist:
	$(CASK) package
