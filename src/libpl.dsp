# Microsoft Developer Studio Project File - Name="libpl" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=libpl - Win32 Release
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "libpl.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "libpl.mak" CFG="libpl - Win32 Release"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libpl - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libpl - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libpl - Win32 Runtime" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "libpl - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\WinRel"
# PROP BASE Intermediate_Dir ".\WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\lib"
# PROP Intermediate_Dir "..\objects\release\libpl"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /GX /O2 /I "win32\uxnt" /I "win32\console" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /FD /c
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
# ADD LINK32 ..\lib\uxnt.lib user32.lib wsock32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /dll /map:"..\bin/libpl.map" /machine:I386 /out:"..\bin/libpl.dll"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Desc=Install header files
PostBuild_Cmds=if not exist ..\include mkdir ..\include	copy pl-itf.h\
             ..\include\SWI-Prolog.h	copy pl-stream.h ..\include\SWI-Stream.h
# End Special Build Tool

!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ".\WinDebug"
# PROP BASE Intermediate_Dir ".\WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "..\lib"
# PROP Intermediate_Dir "..\objects\debug\libpl"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MD /W3 /Gm /GX /Zi /Od /I "win32\uxnt" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /FD /ZI /c
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
# ADD LINK32 ..\lib\uxntD.lib msvcrtd.lib user32.lib wsock32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /nodefaultlib:"msvcrt.lib" /out:"..\bin/libplD.dll"
# SUBTRACT LINK32 /pdb:none
# Begin Special Build Tool
SOURCE=$(InputPath)
PostBuild_Cmds=if not exist ..\include mkdir ..\include	copy pl-itf.h\
             ..\include\SWI-Prolog.h	copy pl-stream.h ..\include\SWI-Stream.h
# End Special Build Tool

!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ".\Win32_Ru"
# PROP BASE Intermediate_Dir ".\Win32_Ru"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "..\runtime"
# PROP Intermediate_Dir "..\objects\runtime\libpl"
# PROP Ignore_Export_Lib 0
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MD /W3 /GX /O2 /I "win32\uxnt" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /D "O_RUNTIME" /YX /FD /c
# SUBTRACT CPP /Fr
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 ..\lib\uxnt.lib user32.lib wsock32.lib advapi32.lib shell32.lib /nologo /subsystem:windows /dll /machine:I386 /nodefaultlib:"libcmt.lib"
# SUBTRACT LINK32 /debug

!ENDIF 

# Begin Target

# Name "libpl - Win32 Release"
# Name "libpl - Win32 Debug"
# Name "libpl - Win32 Runtime"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;hpj;bat;for;f90"
# Begin Source File

SOURCE=.\rc\access.c
# End Source File
# Begin Source File

SOURCE=.\rc\build.c
# End Source File
# Begin Source File

SOURCE=.\rc\html.c
# End Source File
# Begin Source File

SOURCE=".\pl-arith.c"
# End Source File
# Begin Source File

SOURCE=".\pl-atom.c"
# End Source File
# Begin Source File

SOURCE=".\pl-bag.c"
# End Source File
# Begin Source File

SOURCE=".\pl-buffer.c"
# End Source File
# Begin Source File

SOURCE=".\pl-comp.c"
# End Source File
# Begin Source File

SOURCE=".\pl-ctype.c"
# End Source File
# Begin Source File

SOURCE=".\pl-dde.c"
# End Source File
# Begin Source File

SOURCE=".\pl-dwim.c"
# End Source File
# Begin Source File

SOURCE=".\pl-error.c"
# End Source File
# Begin Source File

SOURCE=".\pl-ext.c"
# End Source File
# Begin Source File

SOURCE=".\pl-feature.c"
# End Source File
# Begin Source File

SOURCE=".\pl-file.c"
# End Source File
# Begin Source File

SOURCE=".\pl-flag.c"
# End Source File
# Begin Source File

SOURCE=".\pl-fmt.c"
# End Source File
# Begin Source File

SOURCE=".\pl-funct.c"
# End Source File
# Begin Source File

SOURCE=".\pl-gc.c"
# End Source File
# Begin Source File

SOURCE=".\pl-glob.c"
# End Source File
# Begin Source File

SOURCE=".\pl-itf.c"
# End Source File
# Begin Source File

SOURCE=".\pl-list.c"
# End Source File
# Begin Source File

SOURCE=".\pl-load.c"
# End Source File
# Begin Source File

SOURCE=".\pl-main.c"
# End Source File
# Begin Source File

SOURCE=".\pl-modul.c"
# End Source File
# Begin Source File

SOURCE=".\pl-nt.c"
# End Source File
# Begin Source File

SOURCE=".\pl-op.c"
# End Source File
# Begin Source File

SOURCE=".\pl-os.c"
# End Source File
# Begin Source File

SOURCE=".\pl-prims.c"
# End Source File
# Begin Source File

SOURCE=".\pl-pro.c"
# End Source File
# Begin Source File

SOURCE=".\pl-proc.c"
# End Source File
# Begin Source File

SOURCE=".\pl-prof.c"
# End Source File
# Begin Source File

SOURCE=".\pl-rc.c"
# End Source File
# Begin Source File

SOURCE=".\pl-read.c"
# End Source File
# Begin Source File

SOURCE=".\pl-rec.c"
# End Source File
# Begin Source File

SOURCE=".\pl-setup.c"
# End Source File
# Begin Source File

SOURCE=".\pl-store.c"
# End Source File
# Begin Source File

SOURCE=".\pl-stream.c"
# End Source File
# Begin Source File

SOURCE=".\pl-sys.c"
# End Source File
# Begin Source File

SOURCE=".\pl-table.c"
# End Source File
# Begin Source File

SOURCE=".\pl-term.c"
# End Source File
# Begin Source File

SOURCE=".\pl-thread.c"
# End Source File
# Begin Source File

SOURCE=".\pl-trace.c"
# End Source File
# Begin Source File

SOURCE=".\pl-util.c"
# End Source File
# Begin Source File

SOURCE=".\pl-wam.c"
# End Source File
# Begin Source File

SOURCE=".\pl-wic.c"
# End Source File
# Begin Source File

SOURCE=".\pl-write.c"
# End Source File
# Begin Source File

SOURCE=.\rc\util.c
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE=.\config.h
# End Source File
# Begin Source File

SOURCE=.\rc\html.h
# End Source File
# Begin Source File

SOURCE=".\pl-buffer.h"
# End Source File
# Begin Source File

SOURCE=".\pl-ctype.h"
# End Source File
# Begin Source File

SOURCE=".\pl-data.h"
# End Source File
# Begin Source File

SOURCE=".\pl-error.h"
# End Source File
# Begin Source File

SOURCE=".\pl-funcs.h"
# End Source File
# Begin Source File

SOURCE=".\pl-global.h"
# End Source File
# Begin Source File

SOURCE=".\pl-incl.h"
# End Source File
# Begin Source File

SOURCE=".\pl-itf.h"
# End Source File
# Begin Source File

SOURCE=".\pl-main.h"
# End Source File
# Begin Source File

SOURCE=".\pl-os.h"
# End Source File
# Begin Source File

SOURCE=".\pl-stream.h"
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;cnt;rtf;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\morecore.c
# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=".\pl-alloc.c"
# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=".\pl-atom.ic"
# End Source File
# Begin Source File

SOURCE=".\pl-atom.ih"
# End Source File
# Begin Source File

SOURCE=".\pl-fli.c"
# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1
# End Source File
# Begin Source File

SOURCE=".\pl-funct.ic"
# End Source File
# Begin Source File

SOURCE=".\pl-funct.ih"
# End Source File
# Begin Source File

SOURCE=".\pl-index.c"
# PROP BASE Exclude_From_Build 1
# PROP Exclude_From_Build 1
# End Source File
# End Target
# End Project
