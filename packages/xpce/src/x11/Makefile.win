# X11 		--- X11 implementation of the XPCE Virtual Window Interface

COMMOBJS=	canvas.o fshell.o xcommon.o xconvert.o x11-compat.o
WSTOBJS=	xcolour.o xcursor.o xdisplay.o xdraw.o xevent.o xfont.o \
		xframe.o ximage.o xstream.o xtimer.o xwindow.o x11.o
X11OBJS=	$(COMMOBJS) $(WSTOBJS)

COMMSRC=	$(COMMOBJS:.o=.c)
WSTSRC=		$(WSTOBJS:.o=.c)

objects:	$(X11OBJS)

addlib:		$(X11OBJS)
		$(AR) $(ARFLAGS) $(LIB) $(X11OBJS)
		@touch addlib

$(X11OBJS):	xversion.h

xversion.h:	$(XINCLUDES)/X11/IntrinsicP.h
		egrep '^#[ 	]*define[ 	]+XT_(VERSION|REVISION)' \
		       $(XINCLUDES)/X11/IntrinsicP.h > xversion.h

wstproto.h:	$(WSTSRC)
		$(MKPROTO) $(WSTSRC) | $(CLPROTO) > wstproto.h

proto.h:	$(COMMSRC)
		$(MKPROTO) $(COMMSRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(X11OBJS) xversion.h addlib

.c.o:
		$(CC) -c $(CFLAGS) -I$(XINCLUDES) $*.c
