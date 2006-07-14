################################################################
# Makefile for libtai
#
# Author: Jan Wielemaker
# E-mail: wielemak@science.uva.nl
################################################################

!include ..\rules.mk

LIBOBJ=		tai_add.obj tai_now.obj tai_pack.obj tai_sub.obj \
		tai_unpack.obj taia_add.obj taia_approx.obj \
	        taia_fmtfrac.obj taia_frac.obj taia_half.obj taia_less.obj \
	        taia_now.obj taia_pack.obj taia_sub.obj taia_tai.obj \
		taia_unpack.obj caldate_fmt.obj caldate_scan.obj \
		caldate_fmjd.obj caldate_mjd.obj caldate_norm.obj \
		caldate_ster.obj leapsecs_read.obj \
	        leapsecs_init.obj leapsecs_add.obj leapsecs_sub.obj \
		caltime_fmt.obj caltime_scan.obj caltime_tai.obj \
		 caltime_utc.obj

all:		tai.lib

tai.lib:	$(LIBOBJ)
		if exist $@ del $@
		$(AR) /out:$@ /nologo $(LIBOBJ)

################################################################
# Cleanup
################################################################

clean::
		if exist *.obj del *.obj
		if exist *~ del *~

distclean:	clean
		-del tai.lib *.pdb 2>nul

