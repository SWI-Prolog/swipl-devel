# WIN 		--- Windows and Frames

WINOBJS=	browser.o decorate.o dialog.o display.o \
		displaymanager.o frame.o picture.o resource.o \
		setup.o tile.o view.o window.o
WINSRC=		$(WINOBJS:.o=.c)

objects:	$(WINOBJS)

addlib:		$(WINOBJS)
		$(AR) $(ARFLAGS) $(LIB) $(WINOBJS)
		@touch addlib

proto.h:	$(WINSRC)
		$(MKPROTO) $(WINSRC) | $(CLPROTO) > proto.h

rclass.h:	../../Makefile
		@echo "#define RESOURCE_CLASS \"$(RESOURCE_CLASS)\"" > %$@%
		@if cmp -s %$@% $@; then rm %$@%; else mv %$@% $@; fi

display.o:	rclass.h

clean:
		$(RM) -f *~ a.out core $(WINOBJS) addlib
