.SUFFIXES: .tex .dvi .doc .pl

DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s ../../TeX/makeindex.sty


TEX=	xpceusg.tex \
	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex pceprolog.tex chess.tex \
	trouble.tex memory.tex glossary.tex techniques.tex \
	report.tex font.tex hyper.tex udc.tex dragdrop.tex \
	dragdropdemo.tex control.tex whoiswho.tex namedref.tex \
	ugraphics.tex code.tex resource.tex diaedit.tex dialog.tex \
	wysiwyg.tex frame.tex error.tex application.tex images.tex \
	mlingual.tex summary.tex

LOGO=	figs/uva.eps figs/swi.eps

all:	$(TEX) $(LOGO)
	-$(CORRECTINDEX) xpceusg.idx
	-$(MAKEINDEX) xpceusg
	touch xpceusg.ind
	(TEXINPUTS=../../TeX:$$TEXINPUTS; export TEXINPUTS; latex xpceusg)

tex:	$(TEX)

bib:	xpceusg.aux
	(TEXINPUTS=../../TeX:$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex xpceusg)

figs/uva.ps:	../../TeX/figs/uva.ps
	cp ../../TeX/figs/uva.ps figs/uva.ps

figs/swi.eps:	../../TeX/figs/swi.eps
	cp ../../TeX/figs/swi.eps figs/swi.eps

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
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind *.blg *.log *~ figs/*~ \
	      $(LOGO) figs/class/* summary.tmp
	
