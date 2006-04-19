################################################################
# SWI-Prolog INCLP(R) Package
# Author:    Leslie De Koninck (Leslie.DeKoninck@cs.kuleuven.be)
# Copyright: LGPL (see COPYING or www.gnu.org)
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
CFLAGS=$(CFLAGS) /D__SWI_PROLOG__

INCLPR_PRV=	inclpr_core.pl inclpr_consistency.pl inclpr_inversion.pl \
		inclpr_interval_arithmetic.pl inclpr_symbolic_processing.pl \
		inclpr_natural_interval_extension.pl inclpr_newton.pl \
		inclpr_ordering.pl
LIBPL=		inclpr.pl
IA=		inclpr_interval_arithmetic.dll
EXAMPLES=	benchmarks.pl

LIBDIR=		$(PLBASE)\library\clp
INCLPRDIR=	$(LIBDIR)\inclpr

all:		$(IA)

# todo: linking to math library
%.dll: %.obj	
		$(LD) /dll /out:$@ $(LDFLAGS) $< $(PLLIB)

install:	$(IA)
		copy ia.dll "$(BINDIR)"
		mkdir $(LIBDIR)
		copy $(LIBPL) "$(LIBDIR)"
		mkdir $(INCLPRDIR)
		@for %f in ($(INCLPR_PRV)) do @copy %f "$(INCLPRDIR)"
		$(MAKEINDEX)

################################################################
# Testing
################################################################

check::

################################################################
# Installation
################################################################

uninstall::
		del "$(BINDIR)\ia.dll"
		rmdir "$(INCLPRDIR)"
		del "$(LIBDIR)\inclpr.pl"
		$(MAKEINDEX)

html-install::

xpce-install::

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-DEL *.dll *.lib *.exp *.ilk *.pdb 2>nul


