################################################################
#
################################################################

.SUFFIXES: .tex .dvi .doc .pl

PCEHOME=../../../xpce
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
DOC=pl2cpp
TEX=$(DOC).tex
DVI=$(DOC).dvi
PDF=$(DOC).pdf
LOG=$(DOC).log

################################################################
# Documentation
################################################################

pdf:		$(TEX)
		runtex --pdf $(DOC)

dvi:		$(TEX)
		runtex --dvi $(DOC)

html:		$(TEX)
		latex2html $(DOC)

$(TEX):		$(DOCTOTEX)

.doc.tex:
		$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
		$(PLTOTEX) $*.pl > $*.tex

################################################################
# Clean
################################################################

clean:
		rm -f *~
		rm -f $(TEX)
		runtex --clean

distclean:	clean
		rm -f $(PDF) $(DVI)
