.SUFFIXES: .tex .dvi .doc .pl

PCEHOME=../../..
PDF=relnotes-5.pdf
PS=relnotes-5.ps
DOCTOTEX=$(PCEHOME)/bin/doc2tex
PLTOTEX=$(PCEHOME)/bin/pl2tex
DOC=doc
DVI=$(DOC).dvi
LOG=$(DOC).log

TEX=	$(DOC).tex body.tex tree.tex showdir.tex 

all:	pdf
pdf:	$(PDF)
ps:	$(PS)

$(DVI):	$(TEX)
	env TEXINPUTS=$(PCEHOME)/TeX:: runtex $(DOC)

html:	$(TEX)
	latex2html $(DOC)

$(PS):	$(DVI)
	dvips -o $@ $(DVI)

$(PDF):	$(TEX)
	env TEXINPUTS=$(PCEHOME)/TeX:: runtex --pdf $(DOC)
	mv $(DOC).pdf $@

$(TEX):	$(DOCTOTEX)

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex
.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

clean:
	rm -f $(TEX) \
		*.dvi *.toc *.aux *.idx *.ind *.blg *.log *.ilg \
		*~ figs/*~

	
