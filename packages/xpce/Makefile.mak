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
	@chdir src & $(MAKE) install

uninstall::
	@chdir src & $(MAKE) uninstall

clean::
	@chdir src & $(MAKE) clean

distclean::
	@chdir src & $(MAKE) distclean
