# UNX 		--- Unix File, Process and Network Classes

UNXOBJS=	directory.o file.o process.o socket.o stream.o
UNXSRC=		$(UNXOBJS:.o=.c)

objects:	$(UNXOBJS)

addlib:		$(UNXOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(UNXOBJS)
		@touch addlib

proto.h:	$(UNXSRC)
		$(MKPROTO) $(UNXSRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(UNXOBJS) addlib