.SUFFIXES: .pl .tex .dvi .doc

PLTOTEX=../../../bin/pl2tex
DOCTOTEX=../../../bin/doc2tex
CORRECTINDEX=../../../bin/correctindex
MAKEINDEX=makeindex -s ../../../TeX/makeindex.sty


TEX=	doc.tex \
	attribute.tex canvas.tex draw.tex gesture.tex \
	menu.tex shapes.tex 
		
LOGO=	figs/uva.ps figs/swi.ps

all:	$(TEX) $(LOGO)
	-$(CORRECTINDEX) doc.idx
	-$(MAKEINDEX) doc
	touch doc.ind
	(TEXINPUTS=../../../TeX:$$TEXINPUTS; export TEXINPUTS; latex doc)

$(TEX):	$(PLTOTEX)

bib:	doc.aux
	(TEXINPUTS=../../../TeX:$$TEXINPUTS; \
	 BIBINPUTS=$$TEXINPUTS; \
	 export TEXINPUTS BIBINPUTS; bibtex doc)

figs/uva.ps:	../../../TeX/figs/uva.ps
	-mkdir figs
	cp ../../../TeX/figs/uva.ps figs/uva.ps

figs/swi.ps:	../../../TeX/figs/swi.ps
	cp ../../../TeX/figs/swi.ps figs/swi.ps

.pl.tex:
	$(PLTOTEX) $*.pl > $*.tex

.doc.tex:
	$(DOCTOTEX) $*.doc > $*.tex

clean:
	rm -f $(TEX) *.dvi *.toc *.aux *.idx *.ind *.ilg $(LOGO)
	-rmdir figs
