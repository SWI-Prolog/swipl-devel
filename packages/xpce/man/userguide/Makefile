.SUFFIXES: .tex .dvi .doc .pl

DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s ../../TeX/makeindex.ist


COMMON=	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex pceprolog.tex trouble.tex memory.tex \
	glossary.tex techniques.tex report.tex font.tex hyper.tex \
	udc.tex dragdrop.tex dragdropdemo.tex control.tex whoiswho.tex \
	namedref.tex ugraphics.tex code.tex classvar.tex diaedit.tex \
	dialog.tex wysiwyg.tex frame.tex error.tex application.tex \
	images.tex mlingual.tex summary.tex body.tex

XPCETEX=	xpceusg.tex $(COMMON)
PWTEX=		pwusg.tex pwinfo.tex $(COMMON)

LOGO=	figs/uva.eps figs/swi.eps

all:	$(XPCETEX) $(LOGO) figs/class
	-$(CORRECTINDEX) xpceusg.idx
	-$(MAKEINDEX) xpceusg.idx
	touch xpceusg.ind
	(TEXINPUTS=../../TeX:$$TEXINPUTS; export TEXINPUTS; latex xpceusg)

html:	$(XPCETEX)
	latex2html xpceusg

pwhtml:	$(PWTEX)
	latex2html pwusg

pwtex:	$(PWTEX)
tex:	$(XPCETEX)

bib:	xpceusg.aux
	(TEXINPUTS=../../TeX:$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex xpceusg)

figs/uva.eps:	../../TeX/figs/uva.eps
	cp ../../TeX/figs/uva.eps figs/uva.eps

figs/swi.eps:	../../TeX/figs/swi.eps
	cp ../../TeX/figs/swi.eps figs/swi.eps
figs/class:
	mkdir -p $@

$(TEX):	$(DOCTOTEX)

dragdropdemo.tex:	examples/dragdrop.pl
	$(PLTOTEX) examples/dragdrop.pl > $@
wysiwyg.tex:		examples/wysiwyg.pl
	$(PLTOTEX) examples/wysiwyg.pl > $@
summary.tex:		summary.doc classes.pl
	xpce -g "[classes],process('summary.doc','summary.tmp')" -t halt
	$(DOCTOTEX) summary.tmp > summary.tex
	rm -f summary.tmp

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(XPCETEX) $(PWTEX) \
		*.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~ \
		$(LOGO) figs/class/* summary.tmp
	
