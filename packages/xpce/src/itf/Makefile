# ITF 		--- Host Interface Layer

.SUFFIXES: .cxx

ITF1OBJS=	c.o host.o interface.o
ITF2OBJS=	cpp.o
ITF3OBJS=	action.o query.o send.o get.o main.o xmalloc.o
ITFOBJS=	$(ITF1OBJS) $(ITF2OBJS) $(ITF3OBJS)
ITFSRC=		$(ITF1OBJS:.o=.c) $(ITF2OBJS:.o=.cxx) stub.c

objects:	$(ITFOBJS)

addlib:		$(ITFOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(ITFOBJS)
		@touch addlib

proto.h:	$(ITFSRC)
		$(MKPROTO) $(ITFSRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(ITFOBJS) addlib

.cxx.o:
		$(CPlusPlus) -c $(CFLAGS) -I../../include $*.cxx

action.o:	stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DHOSTACTION $(CFLAGS) $(@:.o=.c)
query.o:	stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DHOSTQUERY $(CFLAGS) $(@:.o=.c)
send.o:		stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DHOSTSEND $(CFLAGS) $(@:.o=.c)
get.o:		stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DHOSTGET $(CFLAGS) $(@:.o=.c)
main.o:		stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DMAIN $(CFLAGS) $(@:.o=.c)
xmalloc.o:	stub.c
		rm -f $(@:.o=.c)
	        ln -s stub.c $(@:.o=.c)
		$(CC) -c -DXMALLOC $(CFLAGS) $(@:.o=.c)
