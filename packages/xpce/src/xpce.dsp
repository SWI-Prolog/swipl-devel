# Microsoft Developer Studio Project File - Name="xpce" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=xpce - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "xpce.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "xpce.mak" CFG="xpce - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "xpce - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "xpce - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "xpce - Win32 Runtime" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "xpce - Win32 Static" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "xpce - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\WinRel"
# PROP BASE Intermediate_Dir ".\WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\pl\lib"
# PROP Intermediate_Dir "..\objects\release\xpce"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "." /I "..\..\pl\include" /I "..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 wsock32.lib xpm.lib ..\..\pl\lib\uxnt.lib ..\..\pl\lib\libpl.lib jpeglib2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /out:"..\..\pl\bin/xpce.dll"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "xpce - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\WinDebug"
# PROP BASE Intermediate_Dir ".\WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\..\pl\lib"
# PROP Intermediate_Dir "..\objects\debug\xpce"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /GX /Zi /Od /I "." /I "..\..\pl\include" /I "..\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /YX /FD /ZI /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 wsock32.lib xpm.lib ..\..\pl\lib\libplD.lib ..\..\pl\lib\uxntD.lib jpeglib2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /nodefaultlib:"libc.lib" /nodefaultlib:"libcd.lib" /out:"..\..\pl\bin/xpceD.dll"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "xpce - Win32 Runtime"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Win32_Ru"
# PROP BASE Intermediate_Dir ".\Win32_Ru"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\..\pl\runtime"
# PROP Intermediate_Dir "..\objects\runtime\xpce"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I ".." /I "." /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /YX /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MD /W3 /GX /O2 /I "." /I "..\..\pl\include" /I "..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /D "O_RUNTIME" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib wsock32.lib c:\jan\lib\uxnt.lib c:\jan\lib\imglib.lib c:\jan\lib\gifread.lib /nologo /subsystem:windows /dll /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 wsock32.lib xpm.lib ..\..\pl\lib\uxnt.lib ..\..\pl\lib\libpl.lib jpeglib2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"libc.lib"
# SUBTRACT LINK32 /pdb:none

!ELSEIF  "$(CFG)" == "xpce - Win32 Static"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\xpce___W"
# PROP BASE Intermediate_Dir ".\xpce___W"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "E:\pl\slib"
# PROP Intermediate_Dir "E:\objects\xpce\release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE CPP /nologo /MD /W3 /GX /O2 /I "." /I "d:\development\pl\include" /I "..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /YX /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MD /W3 /GX /O2 /I "." /I "d:\development\pl\include" /I "..\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "pce_source" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 wsock32.lib xpm.lib uxnt.lib E:\pl\bin\libpl.lib jpeglib2.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# SUBTRACT BASE LINK32 /pdb:none
# ADD LINK32 /nologo /machine:IX86 /out:"e:\pl\lib\xpce.lib"
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "xpce - Win32 Release"
# Name "xpce - Win32 Debug"
# Name "xpce - Win32 Runtime"
# Name "xpce - Win32 Static"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\ker\alloc.c
# End Source File
# Begin Source File

SOURCE=.\msg\and.c
# End Source File
# Begin Source File

SOURCE=.\win\application.c
# End Source File
# Begin Source File

SOURCE=.\gra\arc.c
# End Source File
# Begin Source File

SOURCE=.\adt\area.c
# End Source File
# Begin Source File

SOURCE=.\gra\arrow.c
# End Source File
# Begin Source File

SOURCE=.\itf\asfile.c
# End Source File
# Begin Source File

SOURCE=.\msg\assign.c
# End Source File
# Begin Source File

SOURCE=.\ker\assoc.c
# End Source File
# Begin Source File

SOURCE=.\adt\atable.c
# End Source File
# Begin Source File

SOURCE=.\adt\attribute.c
# End Source File
# Begin Source File

SOURCE=.\ker\behaviour.c
# End Source File
# Begin Source File

SOURCE=.\msg\binding.c
# End Source File
# Begin Source File

SOURCE=.\gra\bitmap.c
# End Source File
# Begin Source File

SOURCE=.\msg\block.c
# End Source File
# Begin Source File

SOURCE=.\adt\bool.c
# End Source File
# Begin Source File

SOURCE=.\gra\box.c
# End Source File
# Begin Source File

SOURCE=.\box\boxes.c
# End Source File
# Begin Source File

SOURCE=.\box\boxes.h
# End Source File
# Begin Source File

SOURCE=.\win\browser.c
# End Source File
# Begin Source File

SOURCE=.\evt\browserselgesture.c
# End Source File
# Begin Source File

SOURCE=.\men\button.c
# End Source File
# Begin Source File

SOURCE=.\itf\c.c
# End Source File
# Begin Source File

SOURCE=.\adt\chain.c
# End Source File
# Begin Source File

SOURCE=.\adt\chaintable.c
# End Source File
# Begin Source File

SOURCE=.\txt\chararray.c
# End Source File
# Begin Source File

SOURCE=.\gra\circle.c
# End Source File
# Begin Source File

SOURCE=.\ker\class.c
# End Source File
# Begin Source File

SOURCE=.\ker\classvar.c
# End Source File
# Begin Source File

SOURCE=.\evt\clickgesture.c
# End Source File
# Begin Source File

SOURCE=.\msg\code.c
# End Source File
# Begin Source File

SOURCE=.\gra\colour.c
# End Source File
# Begin Source File

SOURCE=.\gra\colourmap.c
# End Source File
# Begin Source File

SOURCE=.\gra\connection.c
# End Source File
# Begin Source File

SOURCE=.\evt\conngesture.c
# End Source File
# Begin Source File

SOURCE=.\itf\console.c
# End Source File
# Begin Source File

SOURCE=.\adt\constant.c
# End Source File
# Begin Source File

SOURCE=.\rel\constraint.c
# End Source File
# Begin Source File

SOURCE=.\ker\conversion.c
# End Source File
# Begin Source File

SOURCE=.\itf\cpointer.c
# End Source File
# Begin Source File

SOURCE=.\itf\cpp.cxx
# End Source File
# Begin Source File

SOURCE=.\msg\create.c
# End Source File
# Begin Source File

SOURCE=.\gra\cursor.c
# End Source File
# Begin Source File

SOURCE=.\adt\date.c
# End Source File
# Begin Source File

SOURCE=.\ker\debug.c
# End Source File
# Begin Source File

SOURCE=.\ker\declarations.c
# End Source File
# Begin Source File

SOURCE=.\win\decorate.c
# End Source File
# Begin Source File

SOURCE=.\gra\device.c
# End Source File
# Begin Source File

SOURCE=.\men\diagroup.c
# End Source File
# Begin Source File

SOURCE=.\win\dialog.c
# End Source File
# Begin Source File

SOURCE=.\men\dialogitem.c
# End Source File
# Begin Source File

SOURCE=.\adt\dict.c
# End Source File
# Begin Source File

SOURCE=.\adt\dictitem.c
# End Source File
# Begin Source File

SOURCE=.\unx\directory.c
# End Source File
# Begin Source File

SOURCE=.\win\display.c
# End Source File
# Begin Source File

SOURCE=.\win\displaymgr.c
# End Source File
# Begin Source File

SOURCE=.\gra\draw.c
# End Source File
# Begin Source File

SOURCE=.\txt\editor.c
# End Source File
# Begin Source File

SOURCE=.\evt\edittextgest.c
# End Source File
# Begin Source File

SOURCE=.\gra\elevation.c
# End Source File
# Begin Source File

SOURCE=.\gra\ellipse.c
# End Source File
# Begin Source File

SOURCE=.\msg\equal.c
# End Source File
# Begin Source File

SOURCE=.\ari\equation.c
# End Source File
# Begin Source File

SOURCE=.\ker\error.c
# End Source File
# Begin Source File

SOURCE=.\evt\event.c
# End Source File
# Begin Source File

SOURCE=.\evt\eventnode.c
# End Source File
# Begin Source File

SOURCE=.\evt\eventtree.c
# End Source File
# Begin Source File

SOURCE=.\ari\expression.c
# End Source File
# Begin Source File

SOURCE=.\gra\figure.c
# End Source File
# Begin Source File

SOURCE=.\unx\file.c
# End Source File
# Begin Source File

SOURCE=.\gra\font.c
# End Source File
# Begin Source File

SOURCE=.\gra\format.c
# End Source File
# Begin Source File

SOURCE=.\txt\fragment.c
# End Source File
# Begin Source File

SOURCE=.\win\frame.c
# End Source File
# Begin Source File

SOURCE=.\msg\function.c
# End Source File
# Begin Source File

SOURCE=.\ker\gc.c
# End Source File
# Begin Source File

SOURCE=.\evt\gesture.c
# End Source File
# Begin Source File

SOURCE=.\gnu\getdate.c
# End Source File
# Begin Source File

SOURCE=.\ker\getmethod.c
# End Source File
# Begin Source File

SOURCE=.\img\gifread.c
# End Source File
# Begin Source File

SOURCE=.\img\giftoxpm.c
# End Source File
# Begin Source File

SOURCE=.\ker\glob.c
# End Source File
# Begin Source File

SOURCE=.\ker\global.c
# End Source File
# Begin Source File

SOURCE=.\ker\goodies.c
# End Source File
# Begin Source File

SOURCE=.\gra\graphical.c
# End Source File
# Begin Source File

SOURCE=.\box\grbox.c
# End Source File
# Begin Source File

SOURCE=.\gnu\gregex.c
# End Source File
# Begin Source File

SOURCE=.\gra\handle.c
# End Source File
# Begin Source File

SOURCE=.\evt\handler.c
# End Source File
# Begin Source File

SOURCE=.\evt\handlergroup.c
# End Source File
# Begin Source File

SOURCE=.\adt\hashtable.c
# End Source File
# Begin Source File

SOURCE=.\box\hbox.c
# End Source File
# Begin Source File

SOURCE=.\itf\host.c
# End Source File
# Begin Source File

SOURCE=.\itf\hostdata.c
# End Source File
# Begin Source File

SOURCE=.\rel\hyper.c
# End Source File
# Begin Source File

SOURCE=.\rel\identity.c
# End Source File
# Begin Source File

SOURCE=.\msg\if.c
# End Source File
# Begin Source File

SOURCE=.\gra\image.c
# End Source File
# Begin Source File

SOURCE=.\ker\inline.c
# End Source File
# Begin Source File

SOURCE=.\itf\interface.c
# End Source File
# Begin Source File

SOURCE=.\men\intitem.c
# End Source File
# Begin Source File

SOURCE=.\itf\iostream.c
# End Source File
# Begin Source File

SOURCE=.\img\jdatasrc.c
# End Source File
# Begin Source File

SOURCE=.\gra\joint.c
# End Source File
# Begin Source File

SOURCE=.\img\jpegtoxpm.c
# End Source File
# Begin Source File

SOURCE=.\txt\keybinding.c
# End Source File
# Begin Source File

SOURCE=.\men\label.c
# End Source File
# Begin Source File

SOURCE=.\men\labelbox.c
# End Source File
# Begin Source File

SOURCE=.\fmt\layoutitf.c
# End Source File
# Begin Source File

SOURCE=.\fmt\layoutmgr.c
# End Source File
# Begin Source File

SOURCE=.\box\lbox.c
# End Source File
# Begin Source File

SOURCE=.\gra\line.c
# End Source File
# Begin Source File

SOURCE=.\gra\link.c
# End Source File
# Begin Source File

SOURCE=.\gra\listbrowser.c
# End Source File
# Begin Source File

SOURCE=.\men\menu.c
# End Source File
# Begin Source File

SOURCE=.\men\menubar.c
# End Source File
# Begin Source File

SOURCE=.\men\menuitem.c
# End Source File
# Begin Source File

SOURCE=.\msg\message.c
# End Source File
# Begin Source File

SOURCE=.\ker\method.c
# End Source File
# Begin Source File

SOURCE=.\evt\modifier.c
# End Source File
# Begin Source File

SOURCE=.\evt\movegesture.c
# End Source File
# Begin Source File

SOURCE=.\msw\mscolour.c
# End Source File
# Begin Source File

SOURCE=.\msw\mscommon.c
# End Source File
# Begin Source File

SOURCE=.\msw\mscursor.c
# End Source File
# Begin Source File

SOURCE=.\msw\msdisplay.c
# End Source File
# Begin Source File

SOURCE=.\msw\msdraw.c
# End Source File
# Begin Source File

SOURCE=.\msw\msevent.c
# End Source File
# Begin Source File

SOURCE=.\msw\msfont.c
# End Source File
# Begin Source File

SOURCE=.\msw\msframe.c
# End Source File
# Begin Source File

SOURCE=.\msw\msimage.c
# End Source File
# Begin Source File

SOURCE=.\msw\msmenu.c
# End Source File
# Begin Source File

SOURCE=.\msw\msmetafile.c
# End Source File
# Begin Source File

SOURCE=.\msw\msppm.c
# End Source File
# Begin Source File

SOURCE=.\msw\msprinter.c
# End Source File
# Begin Source File

SOURCE=.\msw\msprocess.c
# End Source File
# Begin Source File

SOURCE=.\msw\msreadimage.c
# End Source File
# Begin Source File

SOURCE=.\msw\msstream.c
# End Source File
# Begin Source File

SOURCE=.\msw\mstimer.c
# End Source File
# Begin Source File

SOURCE=.\msw\mswin.c
# End Source File
# Begin Source File

SOURCE=.\msw\mswindow.c
# End Source File
# Begin Source File

SOURCE=.\evt\mvolgesture.c
# End Source File
# Begin Source File

SOURCE=.\ker\name.c
# End Source File
# Begin Source File

SOURCE=.\msg\nameref.c
# End Source File
# Begin Source File

SOURCE=.\gra\node.c
# End Source File
# Begin Source File

SOURCE=.\msg\nonequal.c
# End Source File
# Begin Source File

SOURCE=.\msg\not.c
# End Source File
# Begin Source File

SOURCE=.\adt\number.c
# End Source File
# Begin Source File

SOURCE=.\ker\object.c
# End Source File
# Begin Source File

SOURCE=.\msg\obtain.c
# End Source File
# Begin Source File

SOURCE=.\prg\operator.c
# End Source File
# Begin Source File

SOURCE=.\msg\or.c
# End Source File
# Begin Source File

SOURCE=.\box\parbox.c
# End Source File
# Begin Source File

SOURCE=.\prg\parser.c
# End Source File
# Begin Source File

SOURCE=.\ker\passing.c
# End Source File
# Begin Source File

SOURCE=.\gra\path.c
# End Source File
# Begin Source File

SOURCE=.\gra\pen.c
# End Source File
# Begin Source File

SOURCE=.\win\picture.c
# End Source File
# Begin Source File

SOURCE=.\gra\pixmap.c
# End Source File
# Begin Source File

SOURCE=.\adt\point.c
# End Source File
# Begin Source File

SOURCE=.\men\popup.c
# End Source File
# Begin Source File

SOURCE=.\evt\popupgesture.c
# End Source File
# Begin Source File

SOURCE=.\gra\postscript.c
# End Source File
# Begin Source File

SOURCE=.\unx\process.c
# End Source File
# Begin Source File

SOURCE=.\msg\progn.c
# End Source File
# Begin Source File

SOURCE=.\ker\programobject.c
# End Source File
# Begin Source File

SOURCE=.\box\proto.h
# End Source File
# Begin Source File

SOURCE=.\msg\quote.c
# End Source File
# Begin Source File

SOURCE=.\itf\rc.c
# End Source File
# Begin Source File

SOURCE=.\adt\real.c
# End Source File
# Begin Source File

SOURCE=.\evt\recogniser.c
# End Source File
# Begin Source File

SOURCE=.\txt\regex.c
# End Source File
# Begin Source File

SOURCE=.\adt\region.c
# End Source File
# Begin Source File

SOURCE=.\rel\relation.c
# End Source File
# Begin Source File

SOURCE=.\evt\resizegesture.c
# End Source File
# Begin Source File

SOURCE=.\evt\resizetabslice.c
# End Source File
# Begin Source File

SOURCE=.\box\rubber.c
# End Source File
# Begin Source File

SOURCE=.\evt\rzolgesture.c
# End Source File
# Begin Source File

SOURCE=.\ker\save.c
# End Source File
# Begin Source File

SOURCE=.\gra\scrollbar.c
# End Source File
# Begin Source File

SOURCE=.\ker\self.c
# End Source File
# Begin Source File

SOURCE=.\ker\sendmethod.c
# End Source File
# Begin Source File

SOURCE=.\win\setup.c
# End Source File
# Begin Source File

SOURCE=.\adt\sheet.c
# End Source File
# Begin Source File

SOURCE=.\adt\size.c
# End Source File
# Begin Source File

SOURCE=.\men\slider.c
# End Source File
# Begin Source File

SOURCE=.\unx\socket.c
# End Source File
# Begin Source File

SOURCE=.\rel\spatial.c
# End Source File
# Begin Source File

SOURCE=.\ker\srclocation.c
# End Source File
# Begin Source File

SOURCE=.\itf\srcsink.c
# End Source File
# Begin Source File

SOURCE=.\txt\str.c
# End Source File
# Begin Source File

SOURCE=.\unx\stream.c
# End Source File
# Begin Source File

SOURCE=.\txt\string.c
# End Source File
# Begin Source File

SOURCE=.\itf\stub.c
# End Source File
# Begin Source File

SOURCE=.\txt\style.c
# End Source File
# Begin Source File

SOURCE=.\txt\syntax.c
# End Source File
# Begin Source File

SOURCE=.\men\tab.c
# End Source File
# Begin Source File

SOURCE=.\fmt\tabcell.c
# End Source File
# Begin Source File

SOURCE=.\fmt\table.c
# End Source File
# Begin Source File

SOURCE=.\fmt\tabslice.c
# End Source File
# Begin Source File

SOURCE=.\men\tabstack.c
# End Source File
# Begin Source File

SOURCE=.\box\tbox.c
# End Source File
# Begin Source File

SOURCE=.\gra\text.c
# End Source File
# Begin Source File

SOURCE=.\txt\textbuffer.c
# End Source File
# Begin Source File

SOURCE=.\txt\textcursor.c
# End Source File
# Begin Source File

SOURCE=.\txt\textimage.c
# End Source File
# Begin Source File

SOURCE=.\men\textitem.c
# End Source File
# Begin Source File

SOURCE=.\txt\textmargin.c
# End Source File
# Begin Source File

SOURCE=.\win\tile.c
# End Source File
# Begin Source File

SOURCE=.\ker\timer.c
# End Source File
# Begin Source File

SOURCE=.\prg\tokeniser.c
# End Source File
# Begin Source File

SOURCE=.\ker\trace.c
# End Source File
# Begin Source File

SOURCE=.\gra\tree.c
# End Source File
# Begin Source File

SOURCE=.\adt\tuple.c
# End Source File
# Begin Source File

SOURCE=.\ker\type.c
# End Source File
# Begin Source File

SOURCE=.\txt\undo.c
# End Source File
# Begin Source File

SOURCE=.\msg\var.c
# End Source File
# Begin Source File

SOURCE=.\ker\variable.c
# End Source File
# Begin Source File

SOURCE=.\adt\vector.c
# End Source File
# Begin Source File

SOURCE=.\win\view.c
# End Source File
# Begin Source File

SOURCE=.\gra\visual.c
# End Source File
# Begin Source File

SOURCE=.\msg\when.c
# End Source File
# Begin Source File

SOURCE=.\msg\while.c
# End Source File
# Begin Source File

SOURCE=.\win\window.c
# End Source File
# Begin Source File

SOURCE=.\ker\xref.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\ker\alloc.h
# End Source File
# Begin Source File

SOURCE=.\itf\c.h
# End Source File
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\gnu\gregex.h
# End Source File
# Begin Source File

SOURCE=.\md.h
# End Source File
# Begin Source File

SOURCE=.\rclass.h
# End Source File
# Begin Source File

SOURCE=.\itf\stub.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
