
# /* SWI-Prolog 1.6        Toplevel Makefile                            */

# /* This is the toplevel makefile for SWI Prolog.                      */
# /* This makefile is almost independent of your operating system!      */
# /* During a first pass we collect all necessary information like      */
# /* paths and compiler directives from your configuration and create   */
# /* a tailor-made version 'xmakefile' of this makefile.                */

# /* Setup your configuration in the following files                    */
# /* ../config.h     system information like paths (global!)            */
# /* md.h            machine dependent information                      */

# Change cc to gcc in the line below if necessary
CPP=cc -E
AWK=awk

# DO NOT CHANGE BELOW THIS LINE

#include "make.h"
CC=M_CC
OPTIMIZE=M_OPTIMIZE
CFLAGS=${OPTIMIZE} M_CFLAGS
LDFLAGS=M_LDFLAGS
LIBS=M_LIBS
#ifdef UNEXEC_SOURCE
UNEXEC=UNEXEC_SOURCE
#else
UNEXEC=gnu/unexec.c
#endif
INSTALL=cp
#ifdef FOREIGN_PL
FOREIGN=FOREIGN_PL
#else
FOREIGN=foreign.pl
#endif

#if OS2MAKE
DUMP=cmd /C .\dump.cmd
#else
DUMP=./dump
#endif

SYSTEMHOME=M_SYSTEMHOME
SYSTEMDIR=M_SYSTEMDIR
DEFSTARTUP=M_DEFSTARTUP
PROLOG=M_PROLOG
BINDIR=M_BINDIR
SEP=M_SEP
SYS=M_SYS

STARTUPDIR=$(SYSTEMHOME)/startup
OS-STARTUPDIR=$(SYSTEMDIR)$(SEP)startup
STARTUPPATH=$(STARTUPDIR)$/startup.MACHINE
OS-STARTUPPATH=$(OS-STARTUPDIR)$(SEP)startup.MACHINE
RUNTIMEDIR=$(SYSTEMHOME)/runtime/MACHINE
OS-RUNTIMEDIR=$(SYSTEMDIR)$(SEP)runtime$(SEP)MACHINE
RUNTIME=$(RUNTIMEDIR)$(SEP)pl.o
LIBRARYDIR=$(SYSTEMHOME)/library
OS-LIBRARYDIR=$(SYSTEMDIR)$(SEP)library

PB=../boot
BINLIB=libPL.a
EXPORTS=../include/SWI-Exports
CINCLUDE=../include/SWI-Prolog.h


OBJ=	pl-wam.o pl-save.o \
	pl-atom.o pl-alloc.o pl-arith.o pl-bag.o pl-comp.o pl-dump.o \
	pl-dwim.o pl-ext.o pl-file.o pl-flag.o pl-fmt.o pl-funct.o \
	pl-gc.o pl-glob.o pl-index.o pl-itf.o pl-list.o pl-load.o \
	pl-modul.o pl-op.o pl-os.o pl-prims.o pl-pro.o pl-proc.o \
	pl-prof.o pl-read.o pl-rec.o pl-setup.o pl-sys.o pl-table.o \
	pl-trace.o pl-util.o pl-wic.o pl-write.o pl-term.o
DEPOBJ=	pl-main.o
EXT=	pl-extend.o

PLINIT=	$(PB)/init.pl
#if OS2MAKE
PLBOOT1=$(PB)/syspred.pl $(PB)/toplevel.pl $(PB)/listing.pl
PLBOOT2=$(PB)/edit.pl $(PB)/sort.pl $(PB)/bags.pl $(PB)/apply.pl
PLBOOT3=$(PB)/list.pl $(PB)/writef.pl $(PB)/history.pl $(PB)/portray.pl
PLBOOT4=$(PB)/profile.pl $(PB)/dwim.pl $(PB)/$(FOREIGN) $(PB)/parms.pl
PLBOOT5=$(PB)/autoload.pl
#else
PLBOOT= $(PB)/syspred.pl $(PB)/toplevel.pl $(PB)/listing.pl \
	$(PB)/edit.pl $(PB)/sort.pl $(PB)/bags.pl $(PB)/apply.pl \
	$(PB)/list.pl $(PB)/writef.pl $(PB)/history.pl $(PB)/portray.pl \
	$(PB)/profile.pl $(PB)/dwim.pl $(PB)/$(FOREIGN) $(PB)/parms.pl \
	$(PB)/autoload.pl
#endif

SRC=	$(OBJ:.o=.c) $(DEPOBJ:.o=.c) $(EXT:.o=.c)

#if OS2MAKE
PARMS=	-DSYSTEMHOME=""$(SYSTEMHOME)"" \
	-DDEFSTARTUP=""$(DEFSTARTUP)""
#else
PARMS=	-DSYSTEMHOME=\"$(SYSTEMHOME)\" \
	-DDEFSTARTUP=\"$(DEFSTARTUP)\"
#endif

TARGETS=	$(EXPORTS) $(CINCLUDE) system startup pl-bite

all:		xmakefile
		make -f xmakefile ${TARGETS}

#if OS2MAKE	/* GNU Make *NEEDS* Unix(tm)-style names as targets! */
startup:	$(SYSTEMHOME) $(STARTUPDIR) $(STARTUPPATH)
#else
startup:	$(SYSTEMDIR) $(OS-STARTUPDIR) $(OS-STARTUPPATH)
#endif
system:		$(SYS)

runtime:	all
		make -f xmakefile xruntime
xruntime:	$(RUNTIME)
		

xmakefile:	Makefile md.h ../config.h
		@-rm -f junk.c make.h
		@sed -f simpl.sed md.h ../config.h > make.h
		@sed -f make.sed Makefile > junk.c
		@$(CPP) junk.c | \
		sed -f strip.sed | \
		sed -n -f extract.sed > xmakefile
		@rm -f junk.c make.h

$(SYS):		$(OBJ) $(DEPOBJ) $(EXT)
		$(CC) $(LDFLAGS) -o $(SYS) $(OBJ) $(DEPOBJ) $(EXT) $(LIBS)

#if OS2MAKE
$(SYSTEMHOME):
#else
$(SYSTEMDIR):
#endif
		-mkdir $(SYSTEMDIR)
#if OS2MAKE
$(STARTUPDIR):
#else
$(OS-STARTUPDIR):	
#endif
		-mkdir $(OS-STARTUPDIR)

#if OS2MAKE
$(STARTUPPATH):		$(PLBOOT) $(PLINIT) pl-wam.o
#else
$(OS-STARTUPPATH):	$(PLBOOT) $(PLINIT) pl-wam.o
#endif
#if OS2MAKE
		echo .\\$(SYS) -O -o $(OS-STARTUPPATH) -b $(PLINIT) -c > xxfoo
		echo $(PLBOOT1) >> xxfoo
		echo $(PLBOOT2) >> xxfoo
		echo $(PLBOOT3) >> xxfoo
		echo $(PLBOOT4) >> xxfoo
		echo $(PLBOOT5) >> xxfoo
		cat xxfoo | sed -e s/$$/\\/ > xxfoo.c
		$(CPP) -P xxfoo.c > boot.cmd
		rm xxfoo.c xxfoo
		cmd /C boot
#else
		./$(SYS) -d 1 -O -o $(OS-STARTUPPATH) -b $(PLINIT) -c $(PLBOOT)
#endif /* OS2MAKE */

$(RUNTIMEDIR):
		@-mkdir $(SYSTEMDIR)$(SEP)runtime
		@-mkdir $@

$(RUNTIME):	$(OBJ) $(DEPOBJ) $(RUNTIMEDIR)
		$(LD) -r -o $@ $(OBJ) $(DEPOBJ)

$(EXPORTS):	xmakefile pl-export
		echo \#\!$(BINDIR)$(SEP)$(PROLOG) > $(EXPORTS)
		cat pl-export >> $(EXPORTS)

$(CINCLUDE):	pl-itf.h
		cp pl-itf.h $(CINCLUDE)


pl-main.o:	xmakefile pl-main.c pl-main.h
		$(CC) -c $(CFLAGS) $(PARMS) pl-main.c

pl-funct.o:	pl-funct.ih
pl-atom.o:	pl-funct.ih
pl-dump.o:	gnu/getpagesize.h $(UNEXEC)
pl-main.o:	Makefile pl-itf.h

pl-funct.ih:	ATOMS
		$(AWK) -f defatom.awk < ATOMS

pl-atom.ih:	ATOMS
		$(AWK) -f defatom.awk < ATOMS

pl-bite:	pl-bite.c
		$(CC) -o pl-bite pl-bite.c

install:	xmakefile
		make -f xmakefile xinstall

xinstall:	$(RUNTIMEDIR) $(LIBRARYDIR)
		@-mkdir $(BINDIR)
#if O_SAVE
		$(INSTALL) $(SYS) $(RUNTIMEDIR)
		$(DUMP) $(OS-RUNTIMEDIR)$(SEP)$(SYS) $(BINDIR)$(SEP)$(PROLOG)
#else
		rm -f $(BINDIR)$(SEP)$(PROLOG)
		
#if O_STORE_PROGRAM && !O_INSTALL_DONOT_STORE
		$(DUMP) $(OS-RUNTIMEDIR)$(SEP)$(SYS) $(BINDIR)$(SEP)$(PROLOG)
#else
		$(INSTALL) $(SYS) $(BINDIR)$(SEP)$(PROLOG)
#endif
#endif
#if OS2MAKE
		$(INSTALL) pl-bite $(BINDIR)$(SEP)pl-bite.exe
#else
		$(INSTALL) pl-bite $(BINDIR)$(SEP)pl-bite
#endif
#if !OS2MAKE
		chmod 755 $(BINDIR)$(SEP)$(PROLOG) $(BINDIR)$(SEP)pl-bite
#endif
$(LIBRARYDIR):
		mkdir $(OS-LIBRARYDIR)
		$(INSTALL) -r ..$(SEP)library$(SEP)* $(OS-LIBRARYDIR)

tags:		TAGS

TAGS:		$(SRC)
		etags $(SRC)

clean:		xmakefile
		make -f xmakefile xclean

xclean:
		rm -f *.o a.out core pl-bite
		rm -f $(SYS)
.c.s:
		$(CC) -S $(CFLAGS) $*.c

.c.o:
		$(CC) -c $(CFLAGS) $*.c

saber_src:
		#load $(OBJ) $(DEPOBJ) $(CFLAGS) $(LIBS)









