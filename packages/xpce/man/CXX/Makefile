.SUFFIXES: .tex .dvi .doc .pl

DOCTOTEX=../../bin/doc2tex
PLTOTEX=../../bin/pl2tex
MAKEINDEX=makeindex -s ../../TeX/makeindex.ist


TEX=	doc.tex pce2cpp.tex

LOGO=	figs/uva.eps figs/swi.eps

all:	$(TEX) $(LOGO)
	-$(MAKEINDEX) doc.idx
	touch doc.ind
	(TEXINPUTS=../../TeX:$$TEXINPUTS; export TEXINPUTS; latex doc)

html:	$(TEX)
	latex2html doc
	mv doc/doc.html doc/Title.html

bib:	doc.aux
	(TEXINPUTS=../../TeX:$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex doc)

figs/uva.eps:	../../TeX/figs/uva.eps
	cp ../../TeX/figs/uva.eps figs/uva.eps

figs/swi.eps:	../../TeX/figs/swi.eps
	cp ../../TeX/figs/swi.eps figs/swi.eps

$(TEX):	$(DOCTOTEX)

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind *.[ib]lg *.log *~ figs/*~ \
	      $(LOGO)
	
