################################################################
# $Id$
#
# Toplevel Makefile for XPCE Version 4.8
#
# NOTE NOTE NOTE: This Makefile only works with GNU-Make!!
# The XPCE project was started by Anjo Anjewierden, who developed version
# 1 and 2.  Version 3 is result of joint effort from Anjo Anjewierden and
# Jan Wielemaker.  Version 4 is written by Jan Wielemaker.
#
# Addresses:	Jan Wielemaker:		jan@swi.psy.uva.nl
#		Anjo Anjewierden:	anjo@swi.psy.uva.nl
#
# 		SWI
#		University of Amsterdam
#		Roetersstraat 15
#		1018 WB  Amsterdam
#		The Netherlands
#		FAX: (+31) 20 5256896
#		Phone: (+31) 20 5256121
#
# Copyright (C) 1994, University of Amsterdam
################################################################

HOST=

all:
ifeq ($(HOST),)
	@if [ ! -z "$$HOSTTYPE" ]; then \
	    echo $(MAKE) HOST=$$HOSTTYPE; \
	    $(MAKE) HOST=$$HOSTTYPE; \
	else \
	    echo "Please type $(MAKE) HOST=<host>"; \
	fi
else
	@if [ ! -d $(HOST) ]; then \
	    mkdir $(HOST); \
	    (cd $(HOST) ; ../src/configure); \
	fi
	cd $(HOST) ; $(MAKE)
endif

	
	
