# ADT 		--- Abstract Data Types

ADTOBJS=	area.o atable.o attribute.o bool.o chain.o chaintable.o \
		constant.o date.o dict.o dictitem.o hashtable.o number.o \
		point.o real.o region.o sheet.o size.o tuple.o vector.o
ADTSRC=		$(ADTOBJS:.o=.c)

objects:	$(ADTOBJS)

addlib:		$(ADTOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(ADTOBJS)
		@touch addlib

proto.h:	$(ADTSRC)
		$(MKPROTO) $(ADTSRC:.o=.c) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ core a.out $(ADTOBJS) addlib
