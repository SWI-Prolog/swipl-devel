# Make the documentation

CP=		ln -fs
CCCP=		gcc -E -C -P
DVITOTTY=	./dvi2tty
DVITOTTY-HOME=	./dvi2tty.src
INDEX=  	makeindex
LATEX=		latex
MANUAL=		./MANUAL.dvi
ONLINE=		../library/MANUAL
ONLINE_INDEX=	../library/helpidx.pl
PL=		../linux/pl

ALLFILES=	Makefile biblio.doc builtin.doc correct_idx correct_ind \
		doc2tex foreign.doc hack.doc intro.doc latex_if_needed  \
		main.doc manual.sty module.doc overview.doc psfig.sty   \
		summary.doc runtime.doc swi.ps figs/tty.sty figs/uva.ps \
		$(DVITOTTY-HOME)

TEXFILES=	manual.sty psfig.sty figs/uva.ps figs/swi.ps          \
		doc.tex intro.tex overview.tex builtin.tex module.tex \
		foreign.tex runtime.tex hack.tex summary.tex xpce.tex

.SUFFIXES:
.SUFFIXES: .doc .tex 

help:		$(ALLFILES)
		@echo
		@echo '"make all"       creates both the online documentation and the manual'
		@echo '"make online"    should build the online documentation'
		@echo '"make manual"    makes the manual'
		@echo
		@echo '"make clean"     removes all temporary files'
		@echo '"make distclean" lets you start from scratch'
		@echo


all:		online manual

dvi:		$(TEXFILES)
		$(LATEX) doc

html:		$(TEXFILES) doc.bbl
		latex2html doc
		mv doc/doc.html doc/Title.html

manual:		$(MANUAL)

online:		$(ONLINE) $(ONLINE_INDEX)

dvi2tty:	$(DVI2TTY)

clean:		
		rm -f *~ *% tmp.* *.aux *.idx *.log *.toc *.ilg
		rm -f online.* *.tex

distclean:	clean
		rm -f $(MANUAL) $(DVITOTTY)
		make -C$(DVITOTTY-HOME) distclean


$(MANUAL):	latex_if_needed doc.tex $(TEXFILES) doc.bbl doc.ind
		./latex_if_needed $(LATEX) doc
		mv doc.dvi $(MANUAL)

man:		doc.tex $(TEXFILES) doc.bbl doc.ind
		$(LATEX) doc

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


$(ONLINE):	$(DVITOTTY) online.dvi
		$(DVITOTTY) -w100 online | cat -s > $(ONLINE)

online.dvi:	latex_if_needed online.tex tty.sty $(TEXFILES)
		./latex_if_needed $(LATEX) online

online.tex:	main.doc
		cp main.doc main.h
		$(CCCP) -DONLINE main.h | cat -s > online.tex
		rm -f main.h

$(ONLINE_INDEX):	$(ONLINE)
		MANUAL=$(ONLINE); INDEX=$(ONLINE_INDEX); export MANUAL INDEX; \
		$(PL) -f none -g online_index -t halt.

$(DVITOTTY):
		$(MAKE) -C$(DVITOTTY-HOME) dvi2tty
		$(CP) $(DVITOTTY-HOME)/dvi2tty ./$(DVITOTTY)

# EOF #
