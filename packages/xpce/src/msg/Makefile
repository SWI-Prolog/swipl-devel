# MSG 		--- Executable (message) Objects

MSGOBJS=	and.o assign.o binding.o block.o code.o create.o \
		equal.o function.o if.o message.o nonequal.o \
		not.o obtain.o or.o progn.o quote.o var.o when.o while.o \
		nameref.o
MSGSRC=		$(MSGOBJS:.o=.c)

objects:	$(MSGOBJS)

addlib:		$(MSGOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(MSGOBJS)
		@touch addlib

proto.h:	$(MSGSRC)
		$(MKPROTO) $(MSGSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(MSGOBJS) addlib
