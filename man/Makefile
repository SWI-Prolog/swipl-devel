# Make the documentation

CP=		ln -fs
CCCP=		/lib/cpp -C -P
DVITOTTY=	dvi2tty
DVITOTTY-HOME=	./dvi2tty.src
INDEX=  	makeindex
LATEX=		latex
MANUAL=		./MANUAL.dvi
ONLINE=		../library/MANUAL
ONLINE_INDEX=	../library/helpidx.pl
PL=		../sun4/pl

ALLFILES=	Makefile biblio.doc builtin.doc correct_idx correct_ind   \
		doc_to_tex foreign.doc hack.doc intro.doc latex_if_needed \
		main.doc manual.sty module.doc overview.doc psfig.sty     \
		summary.doc runtime.doc swi.ps tty.sty uva.ps $(DVITOTTY-HOME)

TEXFILES=	manual.sty psfig.sty uva.ps swi.ps                        \
		intro.tex overview.tex builtin.tex module.tex             \
		foreign.tex runtime.tex hack.tex summary.tex

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

manual:		$(MANUAL)

online:		$(ONLINE) $(ONLINE_INDEX)

dvi2tty:	$(DVI2TTY)

clean:		
		rm -f tmp.* *.aux *.idx *.log *.toc
		rm -f doc.* online.* *.tex

distclean:	clean
		rm -f $(MANUAL) $(DVITOTTY)
		make -C$(DVITOTTY-HOME) distclean


$(MANUAL):	latex_if_needed doc.tex $(TEXFILES) doc.bbl doc.ind
		./latex_if_needed $(LATEX) doc
		mv doc.dvi $(MANUAL)

man:		doc.tex $(TEXFILES) doc.bbl doc.ind
		$(LATEX) doc

doc.tex:	main.doc
		$(CCCP) main.doc | cat -s > doc.tex

summary.tex:	summary.doc
		expand < $*.doc > $*.tex

.doc.tex:	
		./doc_to_tex $*.doc | expand > $*.tex

doc.bbl:	biblio.doc
		expand < biblio.doc > doc.bbl

doc.ind:	latex_if_needed correct_idx correct_ind doc.tex \
		$(TEXFILES) doc.bbl
		./latex_if_needed $(LATEX) doc
		./correct_idx doc.idx > tmp.idx
		$(INDEX) tmp
		./correct_ind tmp.ind > doc.ind


$(ONLINE):	$(DVITOTTY) online.dvi
		$(DVITOTTY) -w100 online | cat -s > $(ONLINE)

online.dvi:	latex_if_needed online.tex tty.sty $(TEXFILES)
		./latex_if_needed $(LATEX) online

online.tex:	main.doc
		$(CCCP) -DONLINE main.doc | cat -s > online.tex

$(ONLINE_INDEX):	$(ONLINE)
		MANUAL=$(ONLINE); INDEX=$(ONLINE_INDEX); export MANUAL INDEX; \
		$(PL) -f none -g online_index -t halt.

$(DVITOTTY):
		$(MAKE) -C$(DVITOTTY-HOME) dvi2tty
		$(CP) $(DVITOTTY-HOME)/dvi2tty ./$(DVITOTTY)

# EOF #
