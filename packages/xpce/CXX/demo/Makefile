# This Makefile is a skeleton for compiling C++ programs for XPCE and
# linking them.  It needs a little configuration:
#
#	PCEHOME must be edited to point to the XPCE home directory
#	PCEARCH must be the machine-directory for the architecture you use
#		You can obtain this value using get(@pce, machine, M).
#
# READ-THIS READ-THIS READ-THIS READ-THIS
#
# G++ 2.4.5 and 2.5.4 (and very likely all others < g++-to-appear) has
# a bug which is `fixed' by preprocessing your input using m4.  Hence
# the -B$(PCEHOME)/include/ flag for g++.  In this directory is a cpp
# csh(1) script that links m4 before g++'s cpp.
#
# This Makefile skeleton is tested for gnu-make and SunOs 4.1.3 /bin/make
#
# Optimization note: using -O takes hardly longer than no optimization and
# reduces the size of the executable a *lot*.  -O is therefore the
# preferred optimisation mode.

.SUFFIXES: .C

PCEHOME=	/staff/jan/src/xpce


CC=gcc
C++=g++
C++FLAGS=	-I$(PCEHOME)/include -O
CFLAGS=		-O
PCELIBS=	$(PCEHOME)/src/XPCE.a -lXt -lX11 -lm
LDFLAGS=	$(PCELIBS)

clean:
		rm -f *.o core a.out *~ *%

# default rules for non GNU-make

.C.o:
		$(C++) -c $(C++FLAGS) $*.C
