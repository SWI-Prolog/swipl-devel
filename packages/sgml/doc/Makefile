################################################################
# Makefile for the documentation of sgml2pl
#
# Author:    Jan Wielemaker
# Created:   Wed Jul 19 2000
# Copyright: GPL
################################################################

LATEX=latex
PDFLATEX=pdflatex
MAKEINDEX=makeindex
DOC=sgml2pl

all:		$(DOC).pdf

$(DOC).tex:	$(DOC).sgml
		./doc2tex $<

$(DOC).pdf:	$(DOC).tex
		$(PDFLATEX) $(DOC)
		$(MAKEINDEX) $(DOC)
		$(PDFLATEX) $(DOC)
		$(MAKEINDEX) $(DOC)
		$(PDFLATEX) $(DOC)

$(DOC).ps:		$(DOC).tex
		$(LATEX) $(DOC)
		$(MAKEINDEX) $(DOC)
		$(LATEX) $(DOC)
		$(MAKEINDEX) $(DOC)
		$(LATEX) $(DOC)
		dvips -o $@ $(DOC)

clean:		
		rm -f $(DOC).aux $(DOC).tex $(DOC).ilg $(DOC).log *~

distclean:	clean
		rm -f $(DOC).pdf $(DOC).ps
