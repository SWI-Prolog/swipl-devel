# GRA 		--- Graphics Classes

GRAOBJS=	arc.o arrow.o bitmap.o box.o circle.o colour.o \
		connection.o cursor.o device.o ellipse.o figure.o \
		font.o format.o graphical.o handle.o image.o \
		joint.o line.o link.o listbrowser.o node.o path.o \
		postscript.o scrollbar.o text.o tree.o visual.o
GRASRC=		$(GRAOBJS:.o=.c)

objects:	$(GRAOBJS)

addlib:		$(GRAOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(GRAOBJS)
		@touch addlib
		
proto.h:	$(GRASRC)
		$(MKPROTO) $(GRASRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(GRAOBJS) addlib
