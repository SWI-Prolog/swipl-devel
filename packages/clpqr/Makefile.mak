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
CLPQDIR=$(CLPDIR)\clpq
CLPQRDIR=$(CLPDIR)\clpqr
PL="$(PLHOME)\bin\plcon.exe"

CLPRPRIV=	bb_r.pl bv_r.pl fourmotz_r.pl ineq_r.pl \
		itf_r.pl nf_r.pl store_r.pl
CLPQPRIV=	bb_q.pl bv_q.pl fourmotz_q.pl ineq_q.pl \
		itf_q.pl nf_q.pl store_q.pl
CLPQRPRIV=	class.pl dump.pl geler.pl itf.pl ordering.pl \
		project.pl redund.pl
LIBPL=		clpr.pl clpq.pl
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
		@if not exist "$(CLPQDIR)\$(NULL)" $(MKDIR) "$(CLPQDIR)"
		@if not exist "$(CLPQRDIR)\$(NULL)" $(MKDIR) "$(CLPQRDIR)"
		@for %f in ($(LIBPL)) do \
		    copy "%f" "$(CLPDIR)"
		@for %f in ($(CLPRPRIV)) do \
		    copy "clpr\%f" "$(CLPRDIR)"
		@for %f in ($(CLPQPRIV)) do \
		    copy "clpq\%f" "$(CLPQDIR)"
		@for %f in ($(CLPQRPRIV)) do \
		    copy "clpqr\%f" "$(CLPQRDIR)"        
		copy README "$(CLPQRDIR)\README.TXT"
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
		@for %f in ($(CLPRPRIV)) do \
		    del "$(CLPRDIR)\%f"
		@for %f in ($(CLPQPRIV)) do \
		    del "$(CLPQDIR)\%f"    
		@for %f in ($(CLPQRPRIV)) do \
		    del "$(CLPQRDIR)\%f"
		del "$(CLPQRDIR)\README.TXT"

clean::
		if exist *~ del *~

distclean:	clean
