# ITF 		--- Host Interface Layer

.SUFFIXES: .cxx

ITF1OBJS=	c.o host.o interface.o cpointer.o asfile.o console.o \
		stub.o xmalloc.o main.o
ITF2OBJS=	cpp.o
ITFOBJS=	$(ITF1OBJS) $(ITF2OBJS)
ITFSRC=		$(ITF1OBJS:.o=.c) $(ITF2OBJS:.o=.cxx)

objects:	$(ITFOBJS)

addlib:		$(ITFOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(ITFOBJS)
		@touch addlib

proto.h:	$(ITFSRC)
		$(MKPROTO) $(ITFSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(ITFOBJS) addlib

.cxx.o:
		$(CXX) -c $(CFLAGS) -I../../include $*.cxx
