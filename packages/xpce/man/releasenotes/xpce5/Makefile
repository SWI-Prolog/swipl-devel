.SUFFIXES: .tex .dvi .doc .pl

PCEHOME=../../..
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
CORRECTINDEX=$(PCEHOME)/bin/correctindex
MAKEINDEX=makeindex -s $(PCEHOME)/TeX/makeindex.ist
DOC=doc

TEX=	$(DOC).tex body.tex tree.tex showdir.tex 

all:	$(TEX)
	-$(CORRECTINDEX) $(DOC).idx
	-$(MAKEINDEX) $(DOC).idx
	touch $(DOC).ind
	(TEXINPUTS=.:$(PCEHOME)/TeX:$$TEXINPUTS; export TEXINPUTS; latex $(DOC))

html:	$(TEX)
	latex2html $(DOC)

$(TEX):	$(DOCTOTEX)

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) \
		*.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~

	
