.SUFFIXES: .tex .dvi .doc .pl

DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s ../../TeX/makeindex.sty


TEX=	doc.tex \
	intro.tex starting.tex globalarch.tex online.tex debug.tex \
	interface.tex graphics.tex pceprolog.tex chess.tex \
	trouble.tex memory.tex glossary.tex	

LOGO=	figs/uva.ps figs/swi.ps

all:	$(TEX) $(LOGO)
	-$(CORRECTINDEX) doc.idx
	-$(MAKEINDEX) doc
	touch doc.ind
	(TEXINPUTS=../../TeX:$$TEXINPUTS; export TEXINPUTS; latex doc)

bib:	doc.aux
	(TEXINPUTS=../../TeX:$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex doc)

figs/uva.ps:	../../TeX/figs/uva.ps
	cp ../../TeX/figs/uva.ps figs/uva.ps

figs/swi.ps:	../../TeX/figs/swi.ps
	cp ../../TeX/figs/swi.ps figs/swi.ps

$(TEX):	$(DOCTOTEX)

chess.tex:	../../prolog/lib/demo/chess.pl $(PLTOTEX)
	$(PLTOTEX) ../../prolog/lib/demo/chess.pl > chess.tex

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind *.blg *.log *~ figs/*~ \
	      $(LOGO)
	
