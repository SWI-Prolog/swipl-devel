################################################################
# Install CLP(R) stuff for the MS-Windows build
# Author: Jan Wielemaker
# 
# Use:
#	nmake /f Makefile.mak
#	nmake /f Makefile.mak install
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk
LIBDIR=$(PLBASE)\library
EXDIR=$(PKGDOC)\examples\clpr
CLPDIR=$(LIBDIR)\clp
CLPRDIR=$(CLPDIR)\clpr
PL="$(PLHOME)\bin\plcon.exe"


CLPRPRIV=	arith_r.pl bb.pl bv.pl class.pl dump.pl fourmotz.pl \
		geler.pl ineq.pl itf3.pl nf.pl nfr.pl ordering.pl \
		project.pl redund.pl store.pl ugraphs.pl
LIBPL=		clpr.pl
EXAMPLES=	

all::
		@echo "Nothing to be done for this package"

check::
#		$(PL) -q -f chr_test.pl -g test,halt -t 'halt(1)'


!IF "$(CFG)" == "rt"
install::
!ELSE
install::
		@if not exist "$(CLPRDIR)\$(NULL)" $(MKDIR) "$(CLPRDIR)"
		@for %f in ($(LIBPL)) do \
		    copy "%f" "$(CLPDIR)"
		@for %f in ($(CLPRPRIV)) do \
		    copy "clpr\%f" "$(CLPRDIR)"
		copy README "$(CLPRDIR)\README.TXT"
!ENDIF

html-install:	install-examples
pdf-install:	install-examples

install-examples::
#		if not exist "$(EXDIR)/$(NULL)" $(MKDIR) "$(EXDIR)"
#		cd examples & @for %f in ($(EXAMPLES)) do @copy %f "$(EXDIR)"

xpce-install::

uninstall::
		@for %f in ($(LIBPL)) do \
		    del "$(CLPDIR)\%f"
		@for %f in ($(CLPDIR)) do \
		    del "$(CLPRDIR)\%f"
		del "$(CLPRDIR)\README.TXT"

clean::
		if exist *~ del *~

distclean:	clean


