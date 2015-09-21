### ====================================================================
### Makefile for University of Utah Sample Thesis
###
### Current target list:
###	all			make .dvi file
###	check			doubled-word, spelling, and syntax checks
###	clean			remove intermediate automatically-created
###				files
###	clobber			same as distclean
###	distclean		remove most automatically-created files
###	dw			make double-word check
###	FRC			dummy target
###	install			install files on system (dummy target)
###	maintainer-clean	remove absolutely everything that make can
###				build
###	mostlyclean		same as clean
###	pdf			make PDF file(s)
###	ps			make PostScript file(s)
###	spell			make spelling check
###	thesis			same as thesis.dvi
###	thesis.aux
###	thesis.dvi
###	thesis.bbl
###	thesis.pdf
###	thesis.ps
###	thesis.toc
###	uninstall		uninstall installed files (dummy target)
###
### [28-May-2001] -- update for LaTeX2e
### [06-Jul-1993]
### ====================================================================

BIBTEX          = bibtex

CP		= cp

DETEX           = detex -n

### Use whichever of these you have:
DISTILL		= ps2pdf
DISTILL		= distill

### The dvips-type1 script is a local wrapper that ensures that Type 1
### outline fonts are used, instead of bitmap fonts, because Acrobat
### Reader does a poor job of displaying the latter.
DVIPS		= dvips-type1

### dw is available at ftp://ftp.math.utah.edu/pub/misc/dw.tar.gz
DW		= dw

FIGURES         = fig1.tex fig2.tex fig3.tex

LACHECK		= lacheck

### Many sites call this latex, with the old 2.09 version available as
### latex209, or not at all.  At the University of Utah Mathematics
### department, latex is always 2.09, and latex2e is 2e.
LATEX           = latex2e

RM              = rm -f

SED             = sed

SHELL		= /bin/sh

TEXFILES        = abstract.tex appendix.tex acknowledge.tex \
		  chap1.tex chap2.tex chap3.tex \
		  thesis.tex
### ====================================================================

all:	thesis

check:	 dw spell syntax-check

clean:
	-$(RM) *.blg
	-$(RM) *.dvi*
	-$(RM) *.dw
	-$(RM) *.ilg
	-$(RM) *.log
	-$(RM) *.o
	-$(RM) *.spell
	-$(RM) *.tmp
	-$(RM) *.old
	-$(RM) *~
	-$(RM) \#*
	-$(RM) core

clobber:	distclean

distclean:	mostlyclean
	-$(RM) *.aux
	-$(RM) *.bbl
	-$(RM) *.idx
	-$(RM) *.ind
	-$(RM) *.toc
	-$(RM) *.lof
	-$(RM) *.lot

dw:
	-$(RM) thesis.dw
	@echo '==================== doubled words ===================='
	for f in $(TEXFILES) ;\
	do \
		echo ===== $$f ===== ; \
		echo ===== $$f ===== >> thesis.dw ; \
		$(DETEX) $$f  | $(DW) >> thesis.dw ; \
	done

### dummy target for force make to make a rule
FRC:

install:
	@echo There is nothing to install for this package

maintainer-clean:	distclean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may require special tools to rebuild."
	-$(RM) thesis.pdf thesis.ps

mostlyclean:	clean

pdf:	thesis.pdf

ps:	thesis.ps

spell:	$(TEXFILES)
	-if [ ! -f thesis.spell-okay ] ; then touch thesis.spell-okay ; fi
	@echo '==================== spelling exceptions ===================='
	for f in $(TEXFILES) ;\
	do \
		echo ===== $$f ===== ; \
		$(DETEX) $$f  | spell +thesis.spell-okay > $$f.spell; \
		if [ -s $$f.spell ] ; \
		then cat $$f.spell ; \
		else $(RM) $$f.spell ; \
		fi ; \
	done

syntax-check:
	$(LACHECK) thesis.tex

thesis:	thesis.dvi

thesis.aux:
	-if [ ! -f thesis.aux ] ; then touch thesis.aux ; fi

thesis.dvi:	thesis.bbl thesis.tex thesis.sty $(FIGURES) $(TEXFILES)
	$(LATEX) thesis.tex
	$(BIBTEX) thesis
	$(MAKE) thesis.toc
	$(LATEX) thesis.tex
	$(MAKE) thesis.toc
	$(LATEX) thesis.tex
	$(MAKE) thesis.toc

### Rigorously, the .bbl file also depends on the .aux file, but since
### that gets create anew each LaTeX run, this causes thesis.dvi to
### be remade unnecessarily:
### thesis.bbl:	thesis.bib thesis.aux
thesis.bbl:	thesis.bib
	-if [ ! -f thesis.aux ] ; then touch thesis.aux ; fi
	-$(BIBTEX) thesis

thesis.pdf:	thesis.ps
	$(DISTILL) $?

thesis.ps:	thesis.dvi
	$(DVIPS) -o $@ $?


### This will strip the sections from appearing in the table of contents.
thesis.toc:	FRC
	$(CP) thesis.toc thesis.toc.old
	$(SED) '/[AB]\.[12]/d' <thesis.toc.old >thesis.toc


uninstall:
	@echo There is nothing to uninstall for this package
