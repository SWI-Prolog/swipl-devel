# Microsoft Visual C++ Generated NMAKE File, Format Version 2.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=Win32 Debug
!MESSAGE No configuration specified.  Defaulting to Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "Win32 Release" && "$(CFG)" != "Win32 Debug" && "$(CFG)" !=\
 "Win32 Runtime"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "libpl.mak" CFG="Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "Win32 Runtime" (based on "Win32 (x86) Dynamic-Link Library")
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

ALL : $(OUTDIR)/libpl.dll $(OUTDIR)/libpl.bsc

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
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /Fp$(OUTDIR)/"libpl.pch"\
 /Fo$(INTDIR)/ /c 
CPP_OBJS=c:\jan\objects\libpl/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libpl.bsc" 

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"libpl.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"libpl.dll" /IMPLIB:$(OUTDIR)/"libpl.lib" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"pl-dwim.obj" \
	$(INTDIR)/"pl-util.obj" \
	$(INTDIR)/"pl-rec.obj" \
	$(INTDIR)/"pl-comp.obj" \
	$(INTDIR)/"pl-write.obj" \
	$(INTDIR)/"pl-table.obj" \
	$(INTDIR)/"pl-pro.obj" \
	$(INTDIR)/"pl-itf.obj" \
	$(INTDIR)/"pl-term.obj" \
	$(INTDIR)/"pl-file.obj" \
	$(INTDIR)/"pl-prims.obj" \
	$(INTDIR)/"pl-dde.obj" \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"pl-bag.obj" \
	$(INTDIR)/"pl-flag.obj" \
	$(INTDIR)/"pl-nt.obj" \
	$(INTDIR)/"pl-ext.obj" \
	$(INTDIR)/"pl-prof.obj" \
	$(INTDIR)/"pl-wam.obj" \
	$(INTDIR)/"pl-sys.obj" \
	$(INTDIR)/"pl-main.obj" \
	$(INTDIR)/"pl-load.obj" \
	$(INTDIR)/"pl-os.obj" \
	$(INTDIR)/"pl-read.obj" \
	$(INTDIR)/"pl-glob.obj" \
	$(INTDIR)/"pl-modul.obj" \
	$(INTDIR)/"pl-trace.obj" \
	$(INTDIR)/"pl-store.obj" \
	$(INTDIR)/"pl-setup.obj" \
	$(INTDIR)/"pl-save.obj" \
	$(INTDIR)/"pl-proc.obj" \
	$(INTDIR)/"pl-dll.obj" \
	$(INTDIR)/"pl-dump.obj" \
	$(INTDIR)/"pl-op.obj" \
	$(INTDIR)/"pl-funct.obj" \
	$(INTDIR)/"pl-list.obj" \
	$(INTDIR)/"pl-fmt.obj" \
	$(INTDIR)/"pl-arith.obj" \
	$(INTDIR)/"pl-fli.obj" \
	$(INTDIR)/"pl-wic.obj" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"pl-gc.obj" \
	$(INTDIR)/"pl-atom.obj"

$(OUTDIR)/libpl.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

ALL : $(OUTDIR)/libpl.dll $(OUTDIR)/libpl.bsc

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
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /Fp$(OUTDIR)/"libpl.pch"\
 /Fo$(INTDIR)/ /Fd$(OUTDIR)/"libpl.pdb" /c 
CPP_OBJS=c:\jan\objects\libpl/
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libpl.bsc" 

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /DEBUG /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:yes /PDB:$(OUTDIR)/"libpl.pdb" /DEBUG /MACHINE:I386\
 /OUT:$(OUTDIR)/"libpl.dll" /IMPLIB:$(OUTDIR)/"libpl.lib" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"pl-dwim.obj" \
	$(INTDIR)/"pl-util.obj" \
	$(INTDIR)/"pl-rec.obj" \
	$(INTDIR)/"pl-comp.obj" \
	$(INTDIR)/"pl-write.obj" \
	$(INTDIR)/"pl-table.obj" \
	$(INTDIR)/"pl-pro.obj" \
	$(INTDIR)/"pl-itf.obj" \
	$(INTDIR)/"pl-term.obj" \
	$(INTDIR)/"pl-file.obj" \
	$(INTDIR)/"pl-prims.obj" \
	$(INTDIR)/"pl-dde.obj" \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"pl-bag.obj" \
	$(INTDIR)/"pl-flag.obj" \
	$(INTDIR)/"pl-nt.obj" \
	$(INTDIR)/"pl-ext.obj" \
	$(INTDIR)/"pl-prof.obj" \
	$(INTDIR)/"pl-wam.obj" \
	$(INTDIR)/"pl-sys.obj" \
	$(INTDIR)/"pl-main.obj" \
	$(INTDIR)/"pl-load.obj" \
	$(INTDIR)/"pl-os.obj" \
	$(INTDIR)/"pl-read.obj" \
	$(INTDIR)/"pl-glob.obj" \
	$(INTDIR)/"pl-modul.obj" \
	$(INTDIR)/"pl-trace.obj" \
	$(INTDIR)/"pl-store.obj" \
	$(INTDIR)/"pl-setup.obj" \
	$(INTDIR)/"pl-save.obj" \
	$(INTDIR)/"pl-proc.obj" \
	$(INTDIR)/"pl-dll.obj" \
	$(INTDIR)/"pl-dump.obj" \
	$(INTDIR)/"pl-op.obj" \
	$(INTDIR)/"pl-funct.obj" \
	$(INTDIR)/"pl-list.obj" \
	$(INTDIR)/"pl-fmt.obj" \
	$(INTDIR)/"pl-arith.obj" \
	$(INTDIR)/"pl-fli.obj" \
	$(INTDIR)/"pl-wic.obj" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"pl-gc.obj" \
	$(INTDIR)/"pl-atom.obj"

$(OUTDIR)/libpl.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "Win32 Runtime"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Win32_Ru"
# PROP BASE Intermediate_Dir "Win32_Ru"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "d:plrt\bin"
# PROP Intermediate_Dir "c:\jan\objects\libpl\runtime"
OUTDIR=d:plrt\bin
INTDIR=c:\jan\objects\libpl\runtime

ALL : $(OUTDIR)/libpl.dll $(OUTDIR)/libpl.bsc

$(OUTDIR) : 
    if not exist $(OUTDIR)/nul mkdir $(OUTDIR)

$(INTDIR) : 
    if not exist $(INTDIR)/nul mkdir $(INTDIR)

# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE CPP /nologo /MT /W3 /GX /YX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /YX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /D "O_RUNTIME" /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /YX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /D "O_RUNTIME"\
 /Fp$(OUTDIR)/"libpl.pch" /Fo$(INTDIR)/ /c 
CPP_OBJS=c:\jan\objects\libpl\runtime/
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
BSC32_SBRS= \
	
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o$(OUTDIR)/"libpl.bsc" 

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL /MACHINE:I386
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib\
 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib\
 odbccp32.lib c:\jan\pl\bin\uxnt.lib /NOLOGO /SUBSYSTEM:windows /DLL\
 /INCREMENTAL:no /PDB:$(OUTDIR)/"libpl.pdb" /MACHINE:I386\
 /OUT:$(OUTDIR)/"libpl.dll" /IMPLIB:$(OUTDIR)/"libpl.lib" 
DEF_FILE=
LINK32_OBJS= \
	$(INTDIR)/"pl-dwim.obj" \
	$(INTDIR)/"pl-util.obj" \
	$(INTDIR)/"pl-rec.obj" \
	$(INTDIR)/"pl-comp.obj" \
	$(INTDIR)/"pl-write.obj" \
	$(INTDIR)/"pl-table.obj" \
	$(INTDIR)/"pl-pro.obj" \
	$(INTDIR)/"pl-itf.obj" \
	$(INTDIR)/"pl-term.obj" \
	$(INTDIR)/"pl-file.obj" \
	$(INTDIR)/"pl-prims.obj" \
	$(INTDIR)/"pl-dde.obj" \
	$(INTDIR)/"pl-stream.obj" \
	$(INTDIR)/"pl-bag.obj" \
	$(INTDIR)/"pl-flag.obj" \
	$(INTDIR)/"pl-nt.obj" \
	$(INTDIR)/"pl-ext.obj" \
	$(INTDIR)/"pl-prof.obj" \
	$(INTDIR)/"pl-wam.obj" \
	$(INTDIR)/"pl-sys.obj" \
	$(INTDIR)/"pl-main.obj" \
	$(INTDIR)/"pl-load.obj" \
	$(INTDIR)/"pl-os.obj" \
	$(INTDIR)/"pl-read.obj" \
	$(INTDIR)/"pl-glob.obj" \
	$(INTDIR)/"pl-modul.obj" \
	$(INTDIR)/"pl-trace.obj" \
	$(INTDIR)/"pl-store.obj" \
	$(INTDIR)/"pl-setup.obj" \
	$(INTDIR)/"pl-save.obj" \
	$(INTDIR)/"pl-proc.obj" \
	$(INTDIR)/"pl-dll.obj" \
	$(INTDIR)/"pl-dump.obj" \
	$(INTDIR)/"pl-op.obj" \
	$(INTDIR)/"pl-funct.obj" \
	$(INTDIR)/"pl-list.obj" \
	$(INTDIR)/"pl-fmt.obj" \
	$(INTDIR)/"pl-arith.obj" \
	$(INTDIR)/"pl-fli.obj" \
	$(INTDIR)/"pl-wic.obj" \
	$(INTDIR)/"pl-buffer.obj" \
	$(INTDIR)/"pl-gc.obj" \
	$(INTDIR)/"pl-atom.obj"

$(OUTDIR)/libpl.dll : $(OUTDIR)  $(DEF_FILE) $(LINK32_OBJS)
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

SOURCE=".\pl-dwim.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-dwim.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-dwim.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-dwim.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-util.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-util.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-util.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-util.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-rec.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-rec.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-rec.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-rec.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-comp.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-comp.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-comp.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-comp.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-write.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-write.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-write.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-write.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-table.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-table.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-table.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-table.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-pro.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-pro.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-pro.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-pro.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-itf.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-itf.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-itf.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-itf.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-term.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-term.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-term.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-term.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-file.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-file.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-file.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-file.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-prims.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-prims.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-prims.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-prims.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dde.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-dde.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-dde.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-dde.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-stream.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-stream.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-stream.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-stream.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-bag.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-bag.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-bag.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-bag.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-flag.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-flag.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-flag.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-flag.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-nt.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-nt.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-nt.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-nt.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-ext.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-ext.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-ext.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-ext.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-prof.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-prof.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-prof.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-prof.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-wam.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-wam.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-wam.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-wam.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-sys.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-sys.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-sys.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-sys.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-main.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-main.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-main.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-main.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-load.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-load.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-load.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-load.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-os.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-os.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-os.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-os.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-read.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-read.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-read.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-read.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-glob.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-glob.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-glob.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-glob.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-modul.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-modul.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-modul.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-modul.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-trace.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-trace.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-trace.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-trace.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-store.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-store.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-store.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-store.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-setup.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-setup.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-setup.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-setup.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-save.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-save.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-save.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-save.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-proc.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-proc.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-proc.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-proc.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dll.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-dll.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-dll.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-dll.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dump.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-dump.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-dump.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-dump.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-op.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-op.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-op.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-op.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-funct.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-funct.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-funct.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-funct.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-list.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-list.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-list.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-list.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-fmt.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-fmt.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-fmt.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-fmt.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-arith.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-arith.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-arith.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-arith.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-fli.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-fli.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-fli.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-fli.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-wic.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-wic.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-wic.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-wic.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-buffer.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-buffer.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-buffer.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-buffer.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-gc.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-gc.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-gc.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-gc.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-atom.c"

!IF  "$(CFG)" == "Win32 Release"

$(INTDIR)/"pl-atom.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Debug"

$(INTDIR)/"pl-atom.obj" :  $(SOURCE)  $(INTDIR)

!ELSEIF  "$(CFG)" == "Win32 Runtime"

$(INTDIR)/"pl-atom.obj" :  $(SOURCE)  $(INTDIR)

!ENDIF 

# End Source File
# End Group
# End Project
################################################################################
