.SUFFIXES: .tex .dvi .doc .pl .eps .pdf

PCETEX=../../TeX
DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s $(PCETEX)/makeindex.ist
DOC=xpceusg
DVI=$(DOC).dvi
LOG=$(DOC).log
PS=$(DOC).ps
PDF=$(DOC).pdf
RERUN="Rerun to get cross-references right"

COMMON=	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex pceprolog.tex trouble.tex memory.tex \
	glossary.tex techniques.tex report.tex font.tex hyper.tex \
	udc.tex dragdrop.tex dragdropdemo.tex control.tex whoiswho.tex \
	namedref.tex ugraphics.tex code.tex classvar.tex diaedit.tex \
	dialog.tex wysiwyg.tex frame.tex error.tex application.tex \
	resource.tex images.tex mlingual.tex summary.tex body.tex \
	mswin.tex

XPCETEX=	xpceusg.tex $(COMMON)
PWTEX=		pwusg.tex pwinfo.tex $(COMMON)

LOGO=	figs/uva.eps figs/swi.eps

all:	$(DVI)
pdf:	$(PDF)
ps:	$(PS)

epsimages=	$(shell find . -name *.eps)
pdfimages=	$(epsimages:.eps=.pdf)

pdfimg:	$(pdfimages)

$(DVI):	$(XPCETEX) $(LOGO)
	@TEXINPUTS=.:$(PCETEX):$$TEXINPUTS; \
	echo "**********************************************************"; \
	echo "* Running latex $(DOC)"; \
	echo "**********************************************************"; \
	export TEXINPUTS; \
	rm -f $(LOG); \
	while test ! -e $(DVI) -o ! -e $(LOG) || grep -q $(RERUN) $(LOG); do \
	  $(CORRECTINDEX) $(DOC).idx; \
	  $(MAKEINDEX) $(DOC).idx; \
	  touch $(DOC).ind; \
	  latex $(DOC) || exit 1; \
	done

$(PDF):	$(XPCETEX) $(LOGO)
	$(MAKE) pdfimg
	@TEXINPUTS=.:$(PCETEX):$$TEXINPUTS; \
	echo "**********************************************************"; \
	echo "* Running pdflatex $(DOC)"; \
	echo "**********************************************************"; \
	export TEXINPUTS; \
	rm -f $(LOG); \
	while test ! -e $(LOG) || grep -q $(RERUN) $(LOG); do \
	  $(CORRECTINDEX) $(DOC).idx; \
	  $(MAKEINDEX) $(DOC).idx; \
	  touch $(DOC).ind; \
	  pdflatex $(DOC) || exit 1; \
	done

$(PS):	$(DVI)
	dvips -o $@ $(DVI)

#$(PDF):	$(PS)
#	pstill -cgipt -o $@ $(PS)

html:	$(XPCETEX)
	latex2html xpceusg

pwhtml:	$(PWTEX)
	latex2html pwusg

pwtex:	$(PWTEX)
tex:	$(XPCETEX)

bib:	xpceusg.aux
	(TEXINPUTS=$(PCETEX):$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex xpceusg)

figs/uva.eps:	$(PCETEX)/figs/uva.eps
	cp $< $@
figs/swi.eps:	$(PCETEX)/figs/swi.eps
	cp $< $@
figs/class:
	mkdir -p $@

$(TEX):	$(DOCTOTEX)

dragdropdemo.tex:	examples/dragdrop.pl
	$(PLTOTEX) examples/dragdrop.pl > $@
wysiwyg.tex:		examples/wysiwyg.pl
	$(PLTOTEX) examples/wysiwyg.pl > $@
summary.tex:		figs/class summary.doc classes.pl
	xpce -f classes.pl -g "process('summary.doc','summary.tmp')" -t halt
	$(DOCTOTEX) summary.tmp > summary.tex
	rm -f summary.tmp

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

.eps.pdf:
	epstopdf $*.eps

clean::
	rm -f $(XPCETEX) $(PWTEX) \
		*.out *.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~ \
		$(LOGO) summary.tmp
	rm -rf figs/class

distclean:	clean
