# Generated automatically from Makefile.in by configure.
# This Makefile is a skeleton for compiling C++ programs for XPCE and
# linking them.  It needs a little configuration:
#
#	PCEHOME must be edited to point to the XPCE home directory
#	PCEARCH must be the machine-directory for the architecture you use
#		You can obtain this value using get(@pce, machine, M).
#
# READ-THIS READ-THIS READ-THIS READ-THIS
#
# This Makefile skeleton is tested for gnu-make and SunOs 4.1.3 /bin/make
#
# Optimization note: using -O takes hardly longer than no optimization and
# reduces the size of the executable a *lot*.  -O is therefore the
# preferred optimisation mode.

# The @Var@ fields are set by configure

.SUFFIXES: .C .so

PCEHOME=/staff/jan/src/xpce
PCEARCH=sun4
CC=gcc
CXX=g++
SO=so
LD=ld
LIBDIR=../../sun4
LDSOFLAGS= -L$(LIBDIR)

all:		pcerc.o
		@echo "This Makefile only provides default rules"

CXXFLAGS=	-I$(PCEHOME)/include -O
CFLAGS=		-O
PCELIBS=	-lXPCEmain -lXPCE
LDFLAGS=	-L$(LIBDIR) $(PCELIBS)

clean:
		rm -f *.o core a.out *~ *%

pcerc.o:	pcerc.c
		$(CC) $(CFLAGS) -o $@ -c $*.c

# default rules for non GNU-make

.C.o:
		$(CXX) -c $(CXXFLAGS) $*.C

.o.so:		
		$(LD) $(LDSOFLAGS) -o $@ -lXPCE pcerc.o $*.o
