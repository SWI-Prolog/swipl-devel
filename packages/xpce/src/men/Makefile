# MEN 		--- Menu (Dialog) items

MENOBJS=	button.o dialogitem.o label.o menu.o menubar.o \
		menuitem.o popup.o slider.o textitem.o
MENSRC=		$(MENOBJS:.o=.c)

objects:	$(MENOBJS)

addlib:		$(MENOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(MENOBJS)
		@touch addlib

proto.h:	$(MENSRC)
		$(MKPROTO) $(MENSRC) | $(CLPROTO) > proto.h

clean:
		rm -f *~ a.out core $(MENOBJS) addlib