# REL 		--- Relation Classes

RELOBJS=	constraint.o hyper.o identity.o relation.o \
		spatial.o
RELSRC=		$(RELOBJS:.o=.c)

objects:	$(RELOBJS)

addlib:		$(RELOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(RELOBJS)
		@touch addlib

proto.h:	$(RELSRC)
		$(MKPROTO) $(RELSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(RELOBJS) addlib
