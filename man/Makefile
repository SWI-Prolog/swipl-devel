# Make the documentation

CP=		ln -fs
CCCP=		gcc -E -C -P
DVITOTTY=	./dvi2tty
DVITOTTY-HOME=	txt/dvi2tty
INDEX=  	makeindex
LATEX=		latex
MANUAL=		./MANUAL.dvi
ONLINE=		../library/MANUAL
ONLINE_INDEX=	../library/helpidx.pl
PL=		../linux/pl

TEXFILES=	doc.tex intro.tex overview.tex builtin.tex module.tex \
		foreign.tex runtime.tex hack.tex summary.tex xpce.tex

.SUFFIXES:
.SUFFIXES: .doc .tex 

help:		
		@echo
		@echo '"make all"       creates both HTML and DVI'
		@echo '"make online"	creates the help/1 database'
		@echo '"make html"      Create HTML verstion'
		@echo '"make manual"    makes the manual'
		@echo
		@echo '"make clean"     removes all temporary files'
		@echo '"make distclean" lets you start from scratch'
		@echo

all:		html manual

dvi:		$(TEXFILES)
		$(LATEX) doc

html:		$(TEXFILES) doc.bbl
		latex2html doc

manual:		$(MANUAL)

online:		$(ONLINE) online_index
online_index:	$(ONLINE_INDEX)

dvi2tty:	$(DVI2TTY)

clean:		
		rm -f *~ *% tmp.* *.aux *.idx *.ind *.log *.toc *.ilg
		rm -f online.* *.tex

distclean:	clean
		rm -f $(MANUAL) $(DVITOTTY)
		make -C$(DVITOTTY-HOME) distclean


$(MANUAL):	latex_if_needed doc.tex $(TEXFILES) doc.bbl doc.ind
		./latex_if_needed $(LATEX) doc
		mv doc.dvi $(MANUAL)

man:		doc.tex $(TEXFILES) doc.bbl doc.ind
		$(LATEX) doc

tex:		$(TEXFILES)

.doc.tex:	
		./doc2tex $*.doc > $*.tex

doc.bbl:	biblio.doc
		cp  biblio.doc $@

htmldoc.bbl:	biblio.doc
		cp  biblio.doc $@

doc.idx:	$(TEXFILES)
		$(LATEX) doc

doc.ind:	doc.idx
		./latex_if_needed $(LATEX) doc
		makeindex doc.idx


$(ONLINE):	online.dvi $(DVITOTTY)
		$(DVITOTTY) -w100 online | \
		cat -s | \
		sed 's/\(.\)_/_\1/g' > $(ONLINE)

online.dvi:	latex_if_needed online.tex $(TEXFILES)
		./latex_if_needed $(LATEX) online

online.doc:	main.doc
		cp main.doc main.h
		$(CCCP) -DONLINE main.h | cat -s > online.doc
		rm -f main.h
doc.doc:	main.doc
		cp main.doc main.h
		$(CCCP) -UONLINE main.h | cat -s > doc.doc
		rm -f main.h

$(ONLINE_INDEX):	$(ONLINE)
		MANUAL=$(ONLINE); INDEX=$(ONLINE_INDEX); export MANUAL INDEX; \
		$(PL) -f none -g '[plindex],online_index' -t halt.

$(DVITOTTY):
		$(MAKE) -C$(DVITOTTY-HOME) dvi2tty
		$(CP) $(DVITOTTY-HOME)/dvi2tty ./$(DVITOTTY)

# EOF #
