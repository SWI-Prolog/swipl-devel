# GNU 		--- GNU-Project Libraries

GNUOBJS=	gregex.o getdate.o
GNUSRC=		$(GNUOBJS:.o=.c)

objects:	$(GNUOBJS)

addlib:		$(GNUOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(GNUOBJS)
		@touch addlib

gregex.o:	gregex.c gregex.h
		$(CC) -c $(COFLAGS) $(CFLAGS) -Dpce_source $*.c
getdate.o:	getdate.c
		$(CC) -c $(CMFLAGS) $(COFLAGS) $(CIFLAGS) $*.c
getdate.c:	getdate.y
		yacc getdate.y
		mv -f y.tab.c getdate.c

proto.h:	$(GNUSRC)
		$(MKPROTO) $(GNUSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(GNUOBJS) getdate.c addlib
