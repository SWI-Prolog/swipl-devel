# TXT 		--- Text Representation and Manipulation Classes

TXTOBJS=	chararray.o editor.o fragment.o keybinding.o \
		regex.o str.o string.o style.o syntax.o \
		textbuffer.o textcursor.o textimage.o \
		textmargin.o undo.o
TXTSRC=		$(TXTOBJS:.o=.c)

objects:	$(TXTOBJS)

addlib:		$(TXTOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(TXTOBJS)
		@touch addlib

proto.h:	$(TXTSRC)
		$(MKPROTO) $(TXTSRC) | $(CLPROTO) > proto.h

clean:
		$(RM) -f *~ a.out core $(TXTOBJS) addlib
