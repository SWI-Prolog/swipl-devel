################################################################
# Hook into the SWI-Prolog package building for building using 
# NMAKE.
#
# The real makefile is in the src directory
################################################################

PLHOME=..\..
!include $(PLHOME)\src\rules.mk

all::
	@chdir src & $(MAKE)

install::
	@chdir src & $(MAKE) $@

html-install::
	@chdir src & $(MAKE) $@

uninstall::
	@chdir src & $(MAKE) $@

clean::
	@chdir src & $(MAKE) $@

distclean::
	@chdir src & $(MAKE) $@
