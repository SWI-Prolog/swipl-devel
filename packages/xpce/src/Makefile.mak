################################################################

PLHOME=..\..\..
!include $(PLHOME)\src\rules.mk

INCLUDE=$(INCLUDE);E:\jan\include;..\include
LIB=$(LIB);E:\jan\lib
XLIBS=$(PLHOME)\lib\uxnt.lib $(PLLIB) jpeglib2.lib xpm.lib comdlg32.lib

XPCEDLL=xpce.dll

all:	$(XPCEDLL)

################################################################
# ADT 		--- Abstract Data Types
################################################################

ADTOBJS=	adt\area.obj adt\atable.obj adt\attribute.obj \
		adt\bool.obj adt\chain.obj adt\chaintable.obj \
		adt\constant.obj adt\date.obj adt\dict.obj \
		adt\dictitem.obj adt\hashtable.obj adt\number.obj \
		adt\point.obj adt\real.obj adt\region.obj \
		adt\sheet.obj adt\size.obj adt\tuple.obj \
		adt\vector.obj

################################################################
# ARI 		--- Arithmetic Operations
################################################################

ARIOBJS=	ari\equation.obj ari\expression.obj

################################################################
# EVT 		--- Event Handling Primitives
################################################################

EVTOBJS=	evt\clickgesture.obj evt\conngesture.obj \
		evt\event.obj evt\eventnode.obj evt\eventtree.obj \
		evt\gesture.obj evt\handler.obj evt\handlergroup.obj \
		evt\modifier.obj evt\movegesture.obj \
		evt\mvolgesture.obj evt\popupgesture.obj \
		evt\recogniser.obj evt\resizegesture.obj \
		evt\rzolgesture.obj evt\edittextgest.obj \
		evt\browserselgesture.obj evt\resizetabslice.obj

################################################################
# GNU 		--- GNU-Project Libraries
################################################################

GNUOBJS=	gnu\gregex.obj gnu\getdate.obj

gnu\gregex.obj:	gnu\gregex.c
		$(CC) -I. -I $(PLHOME)\include $(CFLAGS) /Dpce_source \
			/Fo$@ gnu\gregex.c

################################################################
# GRA 		--- Graphics Classes
################################################################

GRAOBJS=	gra\arc.obj gra\arrow.obj gra\bitmap.obj gra\box.obj \
		gra\circle.obj gra\colour.obj gra\connection.obj \
		gra\cursor.obj gra\device.obj gra\ellipse.obj \
		gra\figure.obj gra\font.obj gra\format.obj \
		gra\graphical.obj gra\handle.obj gra\image.obj \
		gra\joint.obj gra\line.obj gra\link.obj \
		gra\listbrowser.obj gra\node.obj gra\path.obj \
		gra\postscript.obj gra\scrollbar.obj gra\text.obj \
		gra\tree.obj gra\visual.obj gra\pixmap.obj \
		gra\elevation.obj gra\pen.obj gra\draw.obj \
		gra\colourmap.obj gra\bezier.obj

################################################################
# ITF 		--- Host Interface Layer
################################################################

ITFOBJS=	itf\c.obj itf\host.obj itf\interface.obj \
		itf\cpointer.obj itf\asfile.obj itf\console.obj \
		itf\stub.obj itf\xmalloc.obj itf\iostream.obj \
		itf\srcsink.obj itf\rc.obj itf\hostdata.obj \
		itf\cpp.obj

################################################################
# KER 		--- Kernel modules
################################################################

KEROBJS=	ker\alloc.obj ker\assoc.obj ker\behaviour.obj \
		ker\class.obj ker\conversion.obj ker\debug.obj \
		ker\declarations.obj ker\error.obj ker\gc.obj \
		ker\getmethod.obj ker\glob.obj ker\global.obj \
		ker\goodies.obj ker\passing.obj ker\method.obj \
		ker\name.obj ker\object.obj ker\programobject.obj \
		ker\save.obj ker\self.obj ker\sendmethod.obj \
		ker\srclocation.obj ker\timer.obj ker\trace.obj \
		ker\type.obj ker\variable.obj ker\xref.obj \
		ker\classvar.obj ker\inline.obj

################################################################
# MEN 		--- Menu (Dialog) items
################################################################

MENOBJS=	men\button.obj men\dialogitem.obj men\label.obj \
		men\menu.obj men\menubar.obj men\menuitem.obj \
		men\popup.obj men\slider.obj men\textitem.obj \
		men\tab.obj men\diagroup.obj men\tabstack.obj \
		men\labelbox.obj men\intitem.obj

################################################################
# FMT 		--- Layout managers
################################################################

FMTOBJS=	fmt\layoutmgr.obj fmt\layoutitf.obj \
		fmt\table.obj fmt\tabcell.obj fmt\tabslice.obj

################################################################
# BOX 		--- Typesetting stuff
################################################################

BOXOBJS=	box\boxes.obj box\hbox.obj box\tbox.obj \
		box\parbox.obj box\grbox.obj box\rubber.obj \
		box\lbox.obj

################################################################
# MSG 		--- Executable (message) Objects
################################################################

MSGOBJS=	msg\and.obj msg\assign.obj msg\binding.obj \
		msg\block.obj msg\code.obj msg\create.obj \
		msg\equal.obj msg\function.obj msg\if.obj \
		msg\message.obj msg\nonequal.obj msg\not.obj \
		msg\obtain.obj msg\or.obj msg\progn.obj msg\quote.obj \
		msg\var.obj msg\when.obj msg\while.obj \
		msg\nameref.obj

################################################################
# PRG 		--- Language Definition Classes
################################################################

PRGOBJS=	prg\operator.obj prg\parser.obj prg\tokeniser.obj

################################################################
# REL 		--- Relation Classes
################################################################

RELOBJS=	rel\constraint.obj rel\hyper.obj rel\identity.obj \
		rel\relation.obj rel\spatial.obj

################################################################
# TXT 		--- Text Representation and Manipulation Classes
################################################################

TXTOBJS=	txt\chararray.obj txt\editor.obj txt\fragment.obj \
		txt\keybinding.obj txt\regex.obj txt\str.obj \
		txt\string.obj txt\style.obj txt\syntax.obj \
		txt\textbuffer.obj txt\textcursor.obj \
		txt\textimage.obj txt\textmargin.obj txt\undo.obj

################################################################
# UNX 		--- Unix File, Process and Network Classes
################################################################

UNXOBJS=	unx\directory.obj unx\file.obj unx\process.obj \
		unx\socket.obj unx\stream.obj

################################################################
# WIN 		--- Windows and Frames
################################################################

WINOBJS=	win\browser.obj win\decorate.obj win\dialog.obj \
		win\display.obj win\displaymgr.obj win\frame.obj \
		win\picture.obj win\setup.obj win\tile.obj \
		win\view.obj win\window.obj win\application.obj

################################################################
# IMG 		--- Platform independent low-level image stuff
################################################################

IMGOBJS=	img\jdatasrc.obj img\jdatadst.obj img\jpegtoxpm.obj \
		img\gifread.obj img\giftoxpm.obj img\gifwrite.obj

################################################################
# MSW: The MS-Windows binding
################################################################

MSWOBJS=	msw\mscolour.obj msw\msevent.obj msw\msmenu.obj \
		msw\msreadimage.obj \
		msw\mscommon.obj msw\msfont.obj msw\msmetafile.obj \
		msw\msstream.obj msw\mscursor.obj \
		msw\msframe.obj msw\msppm.obj msw\mstimer.obj \
		msw\msdisplay.obj msw\msimage.obj \
		msw\msprinter.obj msw\mswin.obj \
		msw\msdraw.obj msw\msjpeg.obj msw\msprocess.obj \
		msw\mswindow.obj

################################################################
# Join all the objects
################################################################

OBJECTS=	$(ADTOBJS) \
		$(ARIOBJS) \
		$(EVTOBJS) \
		$(GNUOBJS) \
		$(GRAOBJS) \
		$(ITFOBJS) \
		$(KEROBJS) \
		$(MENOBJS) \
		$(FMTOBJS) \
		$(BOXOBJS) \
		$(MSGOBJS) \
		$(PRGOBJS) \
		$(RELOBJS) \
		$(TXTOBJS) \
		$(UNXOBJS) \
		$(WINOBJS) \
		$(IMGOBJS) \
		$(MSWOBJS)

$(XPCEDLL):	$(OBJECTS)
		$(LD) $(LDFLAGS) /out:$@ /dll $(OBJECTS) $(LIBS) $(XLIBS)
	
