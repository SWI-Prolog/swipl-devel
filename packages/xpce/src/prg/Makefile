# PRG 		--- Language Definition Classes

PRGOBJS=	operator.o parser.o tokeniser.o
PRGSRC=		$(PRGOBJS:.o=.c)

objects:	$(PRGOBJS)

addlib:		$(PRGOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(PRGOBJS)
		@touch addlib

proto.h:	$(PRGSRC)
		$(MKPROTO) $(PRGSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(PRGOBJS) addlib
