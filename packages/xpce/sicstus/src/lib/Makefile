################################################################
# Makefile for XPCE/SWI-Prolog compatibility library
################################################################

SO=			so
RES=			sp_compat
TARGETS=		$(addprefix $(RES), .$(SO)) INDEX.pl

all:			$(TARGETS)

sp_compat.$(SO):	sp_compat.c sp_compat.pl ../libswi.a
			splfr sp_compat sp_compat.pl +c sp_compat.c +l -L.. -lswi

sp_compat.s.o:		sp_compat.c sp_compat.pl ../libswi.a
			splfr sp_compat sp_compat.pl +static +c sp_compat.c +l -L.. -lswi

INDEX.pl:		.
			cd .. && sicstus -l mkindex.pl -a lib

clean:
			rm -f *~

distclean:		clean
			rm -f $(TARGETS)
	
