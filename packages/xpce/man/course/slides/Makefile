.SUFFIXES: .tex .dvi .doc .pl

HOME=../../..
DOCTOTEX=$(HOME)/bin/doc2tex
PLTOTEX=$(HOME)/bin/pl2tex
CORRECTINDEX=$(HOME)/bin/correctindex
MAKEINDEX=makeindex -s $(HOME)/TeX/makeindex.sty


TEX=	slides.tex

all:	$(TEX)
	(TEXINPUTS=$(HOME)/TeX:$$TEXINPUTS; export TEXINPUTS; latex slides)

$(TEX):	$(DOCTOTEX)

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind *.[ib]lg *.log *~ figs/*~ \
	      $(LOGO)
	
