# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Application" 0x0101

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "PL.MAK" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Application")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Application")
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
# PROP Intermediate_Dir "c:\jan\objects\pl"
OUTDIR=c:\jan\lib
INTDIR=c:\jan\objects\pl

ALL : $(OUTDIR)/PL.exe $(OUTDIR)/PL.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /YX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /YX /O2 /Ob2 /I "..\..\xnt" /I "..\.." /I "..\..\console" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /YX /O2 /Ob2 /I "..\..\xnt" /I "..\.." /I\
 "..\..\console" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__"\
 /Fp$(OUTDIR)/"PL.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=c:\jan\objects\pl/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"PL.res" /d "NDEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"PL.bsc" 

$(OUTDIR)/PL.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"PL-TRACE.OBJ" \
	$(INTDIR)/"PL-TERM.OBJ" \
	$(INTDIR)/"PL-OP.OBJ" \
	$(INTDIR)/"PL-WRITE.OBJ" \
	$(INTDIR)/"PL-LIST.OBJ" \
	$(INTDIR)/"PL-TABLE.OBJ" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"PL-PRIMS.OBJ" \
	$(INTDIR)/"PL-ATOM.OBJ" \
	$(INTDIR)/"PL-GC.OBJ" \
	$(INTDIR)/"PL-DWIM.OBJ" \
	$(INTDIR)/"pl-extend.obj" \
	$(INTDIR)/"PL-COMP.OBJ" \
	$(INTDIR)/"PL-ARITH.OBJ" \
	$(INTDIR)/"PL-FMT.OBJ" \
	$(INTDIR)/"PL-WIC.OBJ" \
	$(INTDIR)/"PL-FILE.OBJ" \
	$(INTDIR)/"PL-FLAG.OBJ" \
	$(INTDIR)/"PL-REC.OBJ" \
	$(INTDIR)/"PL-SAVE.OBJ" \
	$(INTDIR)/"PL-PRO.OBJ" \
	$(INTDIR)/"PL-ITF.OBJ" \
	$(INTDIR)/"PL-PROF.OBJ" \
	$(INTDIR)/"PL-BAG.OBJ" \
	$(INTDIR)/"PL-MODUL.OBJ" \
	$(INTDIR)/"PL-MAIN.OBJ" \
	$(INTDIR)/"PL-EXT.OBJ" \
	$(INTDIR)/"PL-STORE.OBJ" \
	$(INTDIR)/"PL-SETUP.OBJ" \
	$(INTDIR)/"PL-LOAD.OBJ" \
	$(INTDIR)/"PL-OS.OBJ" \
	$(INTDIR)/"PL-WAM.OBJ" \
	$(INTDIR)/"PL-READ.OBJ" \
	$(INTDIR)/"PL-GLOB.OBJ" \
	$(INTDIR)/"PL-PROC.OBJ" \
	$(INTDIR)/"PL-FUNCT.OBJ" \
	$(INTDIR)/"PL-DUMP.OBJ" \
	$(INTDIR)/"PL-UTIL.OBJ" \
	$(INTDIR)/"PL-SYS.OBJ" \
	$(INTDIR)/"PL-NT.OBJ" \
	$(INTDIR)/"PL-DDE.OBJ" \
	$(INTDIR)/"PL-DLL.OBJ" \
	$(INTDIR)/PL.res \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"PL-FLI.OBJ" \
	$(INTDIR)/"pl-ntmain.obj" \
	$(INTDIR)/"PL-RL.OBJ"
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\lib\readline.lib c:\jan\lib\console.lib c:\jan\lib\uxnt.lib /NOLOGO /SUBSYSTEM:windows /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\lib\readline.lib c:\jan\lib\console.lib c:\jan\lib\uxnt.lib\
 /NOLOGO /SUBSYSTEM:windows /INCREMENTAL:no /PDB:$(OUTDIR)/"PL.pdb"\
 /MACHINE:I386 /OUT:$(OUTDIR)/"PL.exe" 

$(OUTDIR)/PL.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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
# PROP Intermediate_Dir "c:\jan\objects\pl"
OUTDIR=c:\jan\lib
INTDIR=c:\jan\objects\pl

ALL : $(OUTDIR)/PL.exe $(OUTDIR)/PL.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE CPP /nologo /W3 /GX /Zi /YX /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /c
# ADD CPP /nologo /MT /W3 /GX /Zi /YX /Od /I "..\..\xnt" /I "..\.." /I "..\..\console" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /Zi /YX /Od /I "..\..\xnt" /I "..\.." /I\
 "..\..\console" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__"\
 /Fp$(OUTDIR)/"PL.pch" /Fo$(INTDIR)/ /Fd$(OUTDIR)/"PL.pdb" /c 
CPP_OBJS=c:\jan\objects\pl/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
RSC_PROJ=/l 0x409 /fo$(INTDIR)/"PL.res" /d "_DEBUG" 
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"PL.bsc" 

$(OUTDIR)/PL.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"PL-TRACE.OBJ" \
	$(INTDIR)/"PL-TERM.OBJ" \
	$(INTDIR)/"PL-OP.OBJ" \
	$(INTDIR)/"PL-WRITE.OBJ" \
	$(INTDIR)/"PL-LIST.OBJ" \
	$(INTDIR)/"PL-TABLE.OBJ" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"PL-PRIMS.OBJ" \
	$(INTDIR)/"PL-ATOM.OBJ" \
	$(INTDIR)/"PL-GC.OBJ" \
	$(INTDIR)/"PL-DWIM.OBJ" \
	$(INTDIR)/"pl-extend.obj" \
	$(INTDIR)/"PL-COMP.OBJ" \
	$(INTDIR)/"PL-ARITH.OBJ" \
	$(INTDIR)/"PL-FMT.OBJ" \
	$(INTDIR)/"PL-WIC.OBJ" \
	$(INTDIR)/"PL-FILE.OBJ" \
	$(INTDIR)/"PL-FLAG.OBJ" \
	$(INTDIR)/"PL-REC.OBJ" \
	$(INTDIR)/"PL-SAVE.OBJ" \
	$(INTDIR)/"PL-PRO.OBJ" \
	$(INTDIR)/"PL-ITF.OBJ" \
	$(INTDIR)/"PL-PROF.OBJ" \
	$(INTDIR)/"PL-BAG.OBJ" \
	$(INTDIR)/"PL-MODUL.OBJ" \
	$(INTDIR)/"PL-MAIN.OBJ" \
	$(INTDIR)/"PL-EXT.OBJ" \
	$(INTDIR)/"PL-STORE.OBJ" \
	$(INTDIR)/"PL-SETUP.OBJ" \
	$(INTDIR)/"PL-LOAD.OBJ" \
	$(INTDIR)/"PL-OS.OBJ" \
	$(INTDIR)/"PL-WAM.OBJ" \
	$(INTDIR)/"PL-READ.OBJ" \
	$(INTDIR)/"PL-GLOB.OBJ" \
	$(INTDIR)/"PL-PROC.OBJ" \
	$(INTDIR)/"PL-FUNCT.OBJ" \
	$(INTDIR)/"PL-DUMP.OBJ" \
	$(INTDIR)/"PL-UTIL.OBJ" \
	$(INTDIR)/"PL-SYS.OBJ" \
	$(INTDIR)/"PL-NT.OBJ" \
	$(INTDIR)/"PL-DDE.OBJ" \
	$(INTDIR)/"PL-DLL.OBJ" \
	$(INTDIR)/PL.res \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"PL-FLI.OBJ" \
	$(INTDIR)/"pl-ntmain.obj" \
	$(INTDIR)/"PL-RL.OBJ"
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\lib\readline.lib c:\jan\lib\console.lib c:\jan\lib\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\lib\readline.lib c:\jan\lib\console.lib c:\jan\lib\uxnt.lib\
 /NOLOGO /SUBSYSTEM:windows /INCREMENTAL:yes /PDB:$(OUTDIR)/"PL.pdb" /DEBUG\
 /MACHINE:I386 /OUT:$(OUTDIR)/"PL.exe" 

$(OUTDIR)/PL.exe : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=".\PL-TRACE.C"

$(INTDIR)/"PL-TRACE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-TERM.C"

$(INTDIR)/"PL-TERM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-OP.C"

$(INTDIR)/"PL-OP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WRITE.C"

$(INTDIR)/"PL-WRITE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-LIST.C"

$(INTDIR)/"PL-LIST.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-TABLE.C"

$(INTDIR)/"PL-TABLE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-buffer.c"

$(INTDIR)/"pl-buffer.obj" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-PRIMS.C"

$(INTDIR)/"PL-PRIMS.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-ATOM.C"

$(INTDIR)/"PL-ATOM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-GC.C"

$(INTDIR)/"PL-GC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DWIM.C"

$(INTDIR)/"PL-DWIM.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-extend.c"

$(INTDIR)/"pl-extend.obj" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-COMP.C"

$(INTDIR)/"PL-COMP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-ARITH.C"

$(INTDIR)/"PL-ARITH.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FMT.C"

$(INTDIR)/"PL-FMT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WIC.C"

$(INTDIR)/"PL-WIC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FILE.C"

$(INTDIR)/"PL-FILE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FLAG.C"

$(INTDIR)/"PL-FLAG.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-REC.C"

$(INTDIR)/"PL-REC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-SAVE.C"

$(INTDIR)/"PL-SAVE.OBJ" :  $(SOURCE)  $(INTDIR)

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

SOURCE=".\PL-PROF.C"

$(INTDIR)/"PL-PROF.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-BAG.C"

$(INTDIR)/"PL-BAG.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-MODUL.C"

$(INTDIR)/"PL-MODUL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-MAIN.C"

$(INTDIR)/"PL-MAIN.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-EXT.C"

$(INTDIR)/"PL-EXT.OBJ" :  $(SOURCE)  $(INTDIR)

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

SOURCE=".\PL-LOAD.C"

$(INTDIR)/"PL-LOAD.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-OS.C"

$(INTDIR)/"PL-OS.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-WAM.C"

$(INTDIR)/"PL-WAM.OBJ" :  $(SOURCE)  $(INTDIR)

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

SOURCE=".\PL-PROC.C"

$(INTDIR)/"PL-PROC.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FUNCT.C"

$(INTDIR)/"PL-FUNCT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DUMP.C"

$(INTDIR)/"PL-DUMP.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-UTIL.C"

$(INTDIR)/"PL-UTIL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-SYS.C"

$(INTDIR)/"PL-SYS.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-NT.C"

$(INTDIR)/"PL-NT.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DDE.C"

$(INTDIR)/"PL-DDE.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-DLL.C"

$(INTDIR)/"PL-DLL.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=.\PL.RC
DEP_PL_RC=\
	.\PL.ICO\
	.\XPCE.ICO

$(INTDIR)/PL.res :  $(SOURCE)  $(DEP_PL_RC) $(INTDIR)
   $(RSC) $(RSC_PROJ)  $(SOURCE) 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-stream.c"

$(INTDIR)/"pl-stream.obj" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-FLI.C"

$(INTDIR)/"PL-FLI.OBJ" :  $(SOURCE)  $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-ntmain.c"
DEP_PL_NT=\
	".\PL-ITF.H"\
	".\pl-stream.h"\
	\JAN\SRC\CONSOLE\CONSOLE.H

$(INTDIR)/"pl-ntmain.obj" :  $(SOURCE)  $(DEP_PL_NT) $(INTDIR)

# End Source File
################################################################################
# Begin Source File

SOURCE=".\PL-RL.C"
DEP_PL_RL=\
	".\pl-stream.h"\
	".\PL-ITF.H"\
	.\config\win32.h\
	\JAN\SRC\xnt\uxnt.h\
	\JAN\SRC\CONSOLE\CONSOLE.H\
	\JAN\SRC\readline\readline.h\
	\JAN\SRC\readline\keymaps.h\
	\JAN\SRC\readline\proto.h

$(INTDIR)/"PL-RL.OBJ" :  $(SOURCE)  $(DEP_PL_RL) $(INTDIR)

# End Source File
# End Group
# End Project
################################################################################
