.SUFFIXES: .tex .dvi .doc

DOCTOTEX=../../bin/doc2tex
CORRECTINDEX=../../bin/correctindex
MAKEINDEX=makeindex -s ../../TeX/makeindex.sty


TEX=	doc.tex \
	intro.tex data.tex functions.tex examples.tex callback.tex \
	miscellaneous.tex event.tex files.tex
	
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

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

clean:
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind $(LOGO)
