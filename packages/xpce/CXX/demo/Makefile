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
# This Makefile tested on Solaris 2.5
#
# Optimization note: using -O takes hardly longer than no optimization and
# reduces the size of the executable a *lot*.  -O is therefore the
# preferred optimisation mode.

# The @Var@ fields are set by configure

.SUFFIXES: .cxx .so

PCEHOME=../..
PCEARCH=sparc-sunos4.1.3
LIBDIR=$(PCEHOME)/lib/$(PCEARCH)
CC=gcc
CXX=g++
SO=so
LD=g++
LDSOFLAGS=-shared -L$(LIBDIR)
COFLAGS=-O
CMFLAGS= -fPIC -funsigned-char
XLIB=/usr/lib
XLIBS=-lXt -lX11  -ljpeg -lXpm
NETLIBS=
CXXLIBHACK=

PROGS=		view dir drawBoxes east global hello home person \
		showinst test onexit
PLSOLIBS=	bnode.$(SO) dispatch.$(SO) getenv.$(SO) global.$(SO) \
		super.$(SO)

all:		
		@echo Use one of
		@for p in $(PROGS);    do echo "    make $$p"; done
		@echo Or
		@for p in $(PLSOLIBS); do echo "    make $$p"; done
		@echo ""
		@echo "Before running an application, do (in sh)"
		@echo ""
		@echo "    export PCEHOME=$(PCEHOME)"
		@echo "    export LD_LIBRARY_PATH=$(PCEHOME)/lib/$(PCEARCH)"
		@echo ""
		@echo "Or (in csh)"
		@echo ""
		@echo "    setenv PCEHOME $(PCEHOME)"
		@echo "    setenv LD_LIBRARY_PATH $(PCEHOME)/lib/$(PCEARCH)"

CFLAGS=		-I$(PCEHOME)/include $(CMFLAGS) $(COFLAGS)
PCELIBS=	-lXPCEmain -lXPCE $(XLIBS) $(NETLIBS) -lm $(CXXLIBHACK)
LDFLAGS=	-L$(LIBDIR) -L$(XLIB)

clean:
		rm -f *.o *.so core a.out *~ *% $(PROGS)

pcerc.o:	pcerc.c
		$(CC) $(CFLAGS) -o $@ -c $*.c

# Applications

view:		view.o
		$(LD) -o $@ $(LDFLAGS) view.o $(PCELIBS)
dir:		dir.o
		$(LD) -o $@ $(LDFLAGS) dir.o $(PCELIBS)
drawBoxes:	drawBoxes.o
		$(LD) -o $@ $(LDFLAGS) drawBoxes.o $(PCELIBS)
east:		east.o
		$(LD) -o $@ $(LDFLAGS) east.o $(PCELIBS)
global:		global.o
		$(LD) -o $@ $(LDFLAGS) global.o $(PCELIBS)
hello:		hello.o
		$(LD) -o $@ $(LDFLAGS) hello.o $(PCELIBS)
home:		home.o
		$(LD) -o $@ $(LDFLAGS) home.o $(PCELIBS)
onexit:		onexit.o
		$(LD) -o $@ $(LDFLAGS) onexit.o $(PCELIBS)
person:		person.o
		$(LD) -o $@ $(LDFLAGS) person.o $(PCELIBS)
showinst:	showinst.o
		$(LD) -o $@ $(LDFLAGS) showinst.o $(PCELIBS)
test:		test.o
		$(LD) -o $@ $(LDFLAGS) test.o $(PCELIBS)

# default rules for non GNU-make

.cxx.o:
		$(CXX) -c $(CFLAGS) $*.cxx

.o.so:		
		$(LD) $(LDSOFLAGS) -o $@ $*.o
