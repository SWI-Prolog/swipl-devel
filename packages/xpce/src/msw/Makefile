# MSW 		--- MS-Windows implemetation of Virtual Window Interface

MSWOBJS=	colour.o cursor.o display.o draw.o event.o font.o \
		frame.o image.o stream.o timer.o win.o window.o
MSWSRC=		$(MSWOBJS:.o=.c)

objects:	$(MSWOBJS)

addlib:		$(MSWOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(MSWOBJS)
		@touch addlib

proto.h:	$(MSWSRC)
		$(MKPROTO) $(MSWSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(MSWOBJS) addlib
