.SUFFIXES: .tex .dvi .doc .pl

PCEHOME=../..
DOC=doc
PDF=$(DOC).pdf
PS=$(DOC).ps
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
CORRECTINDEX=$(PCEHOME)/bin/correctindex
MAKEINDEX=makeindex -s $(PCEHOME)/TeX/makeindex.ist
DVI=$(DOC).dvi
LOG=$(DOC).log
RERUN="Rerun to get cross-references right"

TEX=	$(DOC).tex body.tex tree.tex showdir.tex 

all:	$(DVI)
pdf:	$(PDF)
ps:	$(PS)

$(DVI):	$(TEX)
	@TEXINPUTS=.:$(PCEHOME)/TeX:$$TEXINPUTS; \
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

html:	$(TEX)
	latex2html $(DOC)

$(PS):	$(DVI)
	dvips -o $@ $(DVI)

$(PDF):	$(PS)
	pstill -cgipt -o $@ $(PS)

$(TEX):	$(DOCTOTEX)

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) \
		*.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~

	
