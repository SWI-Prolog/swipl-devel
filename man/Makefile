################################################################
# Make the documentation
#
# Building the various versions of the documentation requires
# a lot of software to be at the right place.  Generally, please
# pick the required version from the SWI-Prolog home page at
# http://www.swi.psy.uva.nl/projects/SWI-Prolog/
#
# If you insist building it yourself, at least check the following
#
#	* Setting of the $(PL) below
#	* Path to perl in doc2tex
#	* Installed programs
#		- latex 2e
#		- pdflatex
#		- latex2html from the html subdirectory of Prolog
#		- gcc (for ccp as well as building modified dvi2tty)
#
################################################################

PL=		../linux/pl

CP=		ln -fs
CCCP=		gcc -E -C -P
DVITOTTY=	./dvi2tty
DVITOTTY-HOME=	txt/dvi2tty
INDEX=  	makeindex
LATEX=		latex
PDFTEX=		pdflatex
MANUAL=		./MANUAL.dvi
PDF=		MANUAL.pdf
ONLINE=		../library/MANUAL
ONLINE_INDEX=	../library/helpidx.pl

TEXFILES=	doc.tex intro.tex overview.tex builtin.tex module.tex \
		foreign.tex runtime.tex hack.tex summary.tex xpce.tex \
		glossary.tex
STYFILES=	html.sty txt.sty

.SUFFIXES:
.SUFFIXES: .doc .tex 

help:		
		@echo
		@echo '"make all"       creates online, PDF and HTML'
		@echo '"make online"    creates the help/1 database'
		@echo '"make html"      create HTML verstion'
		@echo '"make pdf"       makes $(PDF)'
		@echo
		@echo '"make clean"     removes all temporary files'
		@echo '"make distclean" lets you start from scratch'
		@echo

all:		
		$(MAKE) online
		$(MAKE) clean
		$(MAKE) pdf
		$(MAKE) clean
		$(MAKE) html
		$(MAKE) clean

dvi:		$(TEXFILES)
		$(LATEX) doc

html:		$(TEXFILES) $(STYFILES) doc.bbl
		rm -rf Manual
		latex2html doc

pdf:		$(PDF)

online:		$(ONLINE) online_index
online_index:	$(ONLINE_INDEX)

dvi2tty:	$(DVI2TTY)
txt.sty:	txt/txt.sty
		cp txt/txt.sty $@
html.sty:	../html/html.sty
		cp $< $@

clean:		
		rm -f *~ *% tmp.* *.aux *.idx *.ind *.log *.toc *.ilg
		rm -f online.* *.tex

distclean:	clean
		rm -f $(MANUAL) $(DVITOTTY)
		rm -rf Manual
		make -C$(DVITOTTY-HOME) distclean

$(PDF):		doc.tex $(TEXFILES) doc.bbl
		./runtex --pdf doc
		mv doc.pdf $@

man:		doc.tex $(TEXFILES) doc.bbl doc.ind
		$(LATEX) doc

tex:		$(TEXFILES)

$(TEXFILES):	doc2tex

.doc.tex:	
		./doc2tex $*.doc > $*.tex

doc.bbl:	biblio.doc
		cp  biblio.doc $@

htmldoc.bbl:	biblio.doc
		cp  biblio.doc $@

online.bbl:	biblio.doc
		cp  biblio.doc $@

doc.idx:	$(TEXFILES)
		$(LATEX) doc

$(ONLINE):	online.dvi $(DVITOTTY)
		$(DVITOTTY) -w100 online | \
		cat -s | \
		sed 's/\(.\)_/_\1/g' > $(ONLINE)

online.dvi:	online.tex $(TEXFILES) online.bbl
		./runtex online

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
