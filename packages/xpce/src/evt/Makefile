# EVT 		--- Event Handling Primitives

EVTOBJS=	clickgesture.o connectgesture.o event.o \
		eventnode.o eventtree.o gesture.o handler.o \
		handlergroup.o modifier.o movegesture.o \
		moveoutlinegesture.o popupgesture.o recogniser.o \
		resizegesture.o resizeoutlinegesture.o
EVTSRC=		$(EVTOBJS:.o=.c)

objects:	$(EVTOBJS)

addlib:		$(EVTOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(EVTOBJS)
		@touch addlib

proto.h:	$(EVTSRC)
		$(MKPROTO) $(EVTSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(EVTOBJS) addlib
