# KER 		--- Kernel modules

KER1OBJS=	alloc.o assoc.o behaviour.o class.o conversion.o \
		csymbol.o debug.o declarations.o error.o gc.o \
		getmethod.o glob.o global.o goodies.o passing.o \
		method.o name.o object.o programobject.o save.o \
		self.o sendmethod.o sourcelocation.o timer.o \
		trace.o type.o variable.o vmi.o xref.o error.o
KER2OBJS=	inline.o
KEROBJS=	$(KER1OBJS) $(KER2OBJS)
KER1SRC=	$(KER1OBJS:.o=.c)

objects:	$(KEROBJS)

addlib:		$(KEROBJS)
		$(AR) $(ARFLAGS) $(LIB) $(KEROBJS)
		@touch addlib

proto.h:	$(KER1SRC)
		$(MKPROTO) $(KER1SRC:.o=.c) | $(CLPROTO) > proto.h

version.h:	../../Makefile
		@echo "#define MACHINE \"$(ARCH)\"" > %$@%
		@echo "#define PCE_VERSION \"$(VERSION)\"" >> %$@%
		@echo "#define OS \"$(OS)\"" >> %$@%
		@if cmp -s %$@% $@; then rm %$@%; else mv %$@% $@; fi

self.o:		version.h

name.o:		../h/names.ih

glob.o:		../h/kernel.h ../h/types.h ../h/graphics.h ../h/lang.h

clean:
		rm -f *~ a.out core $(KEROBJS) addlib
