# ARI 		--- Arithmetic Operations

ARIOBJS=	equation.o expression.o
ARISRC=		$(ARIOBJS:.o=.c)

objects:	$(ARIOBJS)

addlib:		$(ARIOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(ARIOBJS)
		@touch addlib

proto.h:	$(ARISRC)
		$(MKPROTO) $(ARISRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(ARIOBJS) addlib