# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "LIBPL.MAK" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

################################################################################
# Begin Project
# PROP Target_Last_Scanned "Win32 Release"
MTL=MkTypLib.exe
CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "c:\jan\lib"
# PROP Intermediate_Dir "c:\jan\objects\libpl"
OUTDIR=c:\jan\lib
INTDIR=c:\jan\objects\libpl

ALL : $(OUTDIR)/LIBPL.dll $(OUTDIR)/LIBPL.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /YX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /YX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /Fp$(OUTDIR)/"LIBPL.pch"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=c:\jan\objects\libpl/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"LIBPL.bsc" 

$(OUTDIR)/LIBPL.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"PL-DWIM.OBJ" \
	$(INTDIR)/"PL-UTIL.OBJ" \
	$(INTDIR)/"PL-REC.OBJ" \
	$(INTDIR)/"PL-COMP.OBJ" \
	$(INTDIR)/"PL-WRITE.OBJ" \
	$(INTDIR)/"PL-TABLE.OBJ" \
	$(INTDIR)/"PL-PRO.OBJ" \
	$(INTDIR)/"PL-ITF.OBJ" \
	$(INTDIR)/"PL-TERM.OBJ" \
	$(INTDIR)/"PL-FILE.OBJ" \
	$(INTDIR)/"PL-PRIMS.OBJ" \
	$(INTDIR)/"PL-DDE.OBJ" \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"PL-BAG.OBJ" \
	$(INTDIR)/"PL-FLAG.OBJ" \
	$(INTDIR)/"PL-NT.OBJ" \
	$(INTDIR)/"PL-EXT.OBJ" \
	$(INTDIR)/"PL-PROF.OBJ" \
	$(INTDIR)/"PL-WAM.OBJ" \
	$(INTDIR)/"PL-SYS.OBJ" \
	$(INTDIR)/"PL-MAIN.OBJ" \
	$(INTDIR)/"PL-LOAD.OBJ" \
	$(INTDIR)/"PL-OS.OBJ" \
	$(INTDIR)/"PL-READ.OBJ" \
	$(INTDIR)/"PL-GLOB.OBJ" \
	$(INTDIR)/"PL-MODUL.OBJ" \
	$(INTDIR)/"PL-TRACE.OBJ" \
	$(INTDIR)/"PL-STORE.OBJ" \
	$(INTDIR)/"PL-SETUP.OBJ" \
	$(INTDIR)/"PL-SAVE.OBJ" \
	$(INTDIR)/"PL-PROC.OBJ" \
	$(INTDIR)/"PL-DLL.OBJ" \
	$(INTDIR)/"PL-DUMP.OBJ" \
	$(INTDIR)/"PL-OP.OBJ" \
	$(INTDIR)/"PL-FUNCT.OBJ" \
	$(INTDIR)/"PL-LIST.OBJ" \
	$(INTDIR)/"PL-FMT.OBJ" \
	$(INTDIR)/"PL-ARITH.OBJ" \
	$(INTDIR)/"PL-FLI.OBJ" \
	$(INTDIR)/"PL-WIC.OBJ" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"PL-GC.OBJ" \
	$(INTDIR)/"PL-ATOM.OBJ"
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"LIBPL.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"LIBPL.dll" /IMPLIB:$(OUTDIR)/"LIBPL.lib" 

$(OUTDIR)/LIBPL.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "c:\jan\lib"
# PROP Intermediate_Dir "c:\jan\objects\libpl"
OUTDIR=c:\jan\lib
INTDIR=c:\jan\objects\libpl

ALL : $(OUTDIR)/LIBPL.dll $(OUTDIR)/LIBPL.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /Zi /YX /Od /I "c:\jan\pl\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /Zi /YX /Od /I "c:\jan\pl\include" /D "_DEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /Fp$(OUTDIR)/"LIBPL.pch"\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"LIBPL.pdb" /c 
CPP_OBJS=c:\jan\objects\libpl/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"LIBPL.bsc" 

$(OUTDIR)/LIBPL.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"PL-DWIM.OBJ" \
	$(INTDIR)/"PL-UTIL.OBJ" \
	$(INTDIR)/"PL-REC.OBJ" \
	$(INTDIR)/"PL-COMP.OBJ" \
	$(INTDIR)/"PL-WRITE.OBJ" \
	$(INTDIR)/"PL-TABLE.OBJ" \
	$(INTDIR)/"PL-PRO.OBJ" \
	$(INTDIR)/"PL-ITF.OBJ" \
	$(INTDIR)/"PL-TERM.OBJ" \
	$(INTDIR)/"PL-FILE.OBJ" \
	$(INTDIR)/"PL-PRIMS.OBJ" \
	$(INTDIR)/"PL-DDE.OBJ" \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"PL-BAG.OBJ" \
	$(INTDIR)/"PL-FLAG.OBJ" \
	$(INTDIR)/"PL-NT.OBJ" \
	$(INTDIR)/"PL-EXT.OBJ" \
	$(INTDIR)/"PL-PROF.OBJ" \
	$(INTDIR)/"PL-WAM.OBJ" \
	$(INTDIR)/"PL-SYS.OBJ" \
	$(INTDIR)/"PL-MAIN.OBJ" \
	$(INTDIR)/"PL-LOAD.OBJ" \
	$(INTDIR)/"PL-OS.OBJ" \
	$(INTDIR)/"PL-READ.OBJ" \
	$(INTDIR)/"PL-GLOB.OBJ" \
	$(INTDIR)/"PL-MODUL.OBJ" \
	$(INTDIR)/"PL-TRACE.OBJ" \
	$(INTDIR)/"PL-STORE.OBJ" \
	$(INTDIR)/"PL-SETUP.OBJ" \
	$(INTDIR)/"PL-SAVE.OBJ" \
	$(INTDIR)/"PL-PROC.OBJ" \
	$(INTDIR)/"PL-DLL.OBJ" \
	$(INTDIR)/"PL-DUMP.OBJ" \
	$(INTDIR)/"PL-OP.OBJ" \
	$(INTDIR)/"PL-FUNCT.OBJ" \
	$(INTDIR)/"PL-LIST.OBJ" \
	$(INTDIR)/"PL-FMT.OBJ" \
	$(INTDIR)/"PL-ARITH.OBJ" \
	$(INTDIR)/"PL-FLI.OBJ" \
	$(INTDIR)/"PL-WIC.OBJ" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"PL-GC.OBJ" \
	$(INTDIR)/"PL-ATOM.OBJ"
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"LIBPL.pdb" /DEBUG /MACHINE:I386\
 /OUT:$(OUTDIR)/"LIBPL.dll" /IMPLIB:$(OUTDIR)/"LIBPL.lib" 

$(OUTDIR)/LIBPL.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.c{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_OBJS)}.obj:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Group "Source Files"

################################################################################
# Begin Source File

SOURCE=".\PL-DWIM.C"

$(INTDIR)/"PL-DWIM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-UTIL.C"

$(INTDIR)/"PL-UTIL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-REC.C"

$(INTDIR)/"PL-REC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-COMP.C"

$(INTDIR)/"PL-COMP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WRITE.C"

$(INTDIR)/"PL-WRITE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-TABLE.C"

$(INTDIR)/"PL-TABLE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-PRO.C"

$(INTDIR)/"PL-PRO.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-ITF.C"

$(INTDIR)/"PL-ITF.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-TERM.C"

$(INTDIR)/"PL-TERM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FILE.C"
DEP_PL_FI=\
	".\pl-incl.h"\
	".\pl-ctype.h"\
	c:\jan\pl\include\console.h\
	c:\jan\pl\include\uxnt.h\
	".\pl-stream.h"\
	".\pl-data.h"\
	".\pl-itf.h"\
	".\pl-os.h"\
	".\pl-funcs.h"\
	".\pl-main.h"\
	".\pl-atom.ih"\
	".\pl-funct.ih"\
	".\pl-buffer.h"

$(INTDIR)/"PL-FILE.OBJ" :  $(SOURCE)  $(DEP_PL_FI) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-PRIMS.C"

$(INTDIR)/"PL-PRIMS.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DDE.C"

$(INTDIR)/"PL-DDE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-stream.c"

$(INTDIR)/"pl-stream.obj" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-BAG.C"

$(INTDIR)/"PL-BAG.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FLAG.C"

$(INTDIR)/"PL-FLAG.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-NT.C"

$(INTDIR)/"PL-NT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-EXT.C"
DEP_PL_EX=\
	".\pl-incl.h"\
	c:\jan\pl\include\uxnt.h\
	".\pl-stream.h"\
	".\pl-data.h"\
	".\pl-itf.h"\
	".\pl-os.h"\
	".\pl-funcs.h"\
	".\pl-main.h"\
	".\pl-atom.ih"\
	".\pl-funct.ih"\
	".\pl-buffer.h"

$(INTDIR)/"PL-EXT.OBJ" :  $(SOURCE)  $(DEP_PL_EX) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-PROF.C"

$(INTDIR)/"PL-PROF.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WAM.C"

$(INTDIR)/"PL-WAM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-SYS.C"

$(INTDIR)/"PL-SYS.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-MAIN.C"

$(INTDIR)/"PL-MAIN.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-LOAD.C"

$(INTDIR)/"PL-LOAD.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-OS.C"
DEP_PL_OS=\
	".\pl-incl.h"\
	".\pl-ctype.h"\
	c:\jan\pl\include\uxnt.h\
	".\pl-stream.h"\
	".\pl-data.h"\
	".\pl-itf.h"\
	".\pl-os.h"\
	".\pl-funcs.h"\
	".\pl-main.h"\
	".\pl-atom.ih"\
	".\pl-funct.ih"\
	".\pl-buffer.h"

$(INTDIR)/"PL-OS.OBJ" :  $(SOURCE)  $(DEP_PL_OS) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-READ.C"

$(INTDIR)/"PL-READ.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-GLOB.C"

$(INTDIR)/"PL-GLOB.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-MODUL.C"

$(INTDIR)/"PL-MODUL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-TRACE.C"

$(INTDIR)/"PL-TRACE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-STORE.C"

$(INTDIR)/"PL-STORE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-SETUP.C"

$(INTDIR)/"PL-SETUP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-SAVE.C"

$(INTDIR)/"PL-SAVE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-PROC.C"

$(INTDIR)/"PL-PROC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DLL.C"

$(INTDIR)/"PL-DLL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DUMP.C"

$(INTDIR)/"PL-DUMP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-OP.C"

$(INTDIR)/"PL-OP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FUNCT.C"

$(INTDIR)/"PL-FUNCT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-LIST.C"

$(INTDIR)/"PL-LIST.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FMT.C"

$(INTDIR)/"PL-FMT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-ARITH.C"

$(INTDIR)/"PL-ARITH.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FLI.C"
DEP_PL_FL=\
	".\pl-incl.h"\
	c:\jan\pl\include\uxnt.h\
	".\pl-stream.h"\
	".\pl-data.h"\
	".\pl-itf.h"\
	".\pl-os.h"\
	".\pl-funcs.h"\
	".\pl-main.h"\
	".\pl-atom.ih"\
	".\pl-funct.ih"\
	".\pl-buffer.h"

$(INTDIR)/"PL-FLI.OBJ" :  $(SOURCE)  $(DEP_PL_FL) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WIC.C"

$(INTDIR)/"PL-WIC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-buffer.c"

$(INTDIR)/"pl-buffer.obj" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-GC.C"

$(INTDIR)/"PL-GC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-ATOM.C"

$(INTDIR)/"PL-ATOM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
