# Microsoft Developer Studio Generated NMAKE File, Format Version 4.20
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

!IF "$(CFG)" == ""
CFG=libpl - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to libpl - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "libpl - Win32 Release" && "$(CFG)" != "libpl - Win32 Debug" &&\
 "$(CFG)" != "libpl - Win32 Runtime"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "libpl.mak" CFG="libpl - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "libpl - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libpl - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "libpl - Win32 Runtime" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "libpl - Win32 Release"
CPP=cl.exe
MTL=mktyplib.exe
RSC=rc.exe

!IF  "$(CFG)" == "libpl - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "WinRel"
# PROP BASE Intermediate_Dir "WinRel"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "d:\development\lib"
# PROP Intermediate_Dir "d:\development\objects\pl"
OUTDIR=d:\development\lib
INTDIR=d:\development\objects\pl

ALL : "$(OUTDIR)\libpl.dll"

CLEAN : 
	-@erase "$(INTDIR)\pl-arith.obj"
	-@erase "$(INTDIR)\pl-atom.obj"
	-@erase "$(INTDIR)\pl-bag.obj"
	-@erase "$(INTDIR)\pl-buffer.obj"
	-@erase "$(INTDIR)\pl-comp.obj"
	-@erase "$(INTDIR)\pl-dde.obj"
	-@erase "$(INTDIR)\pl-dll.obj"
	-@erase "$(INTDIR)\pl-dump.obj"
	-@erase "$(INTDIR)\pl-dwim.obj"
	-@erase "$(INTDIR)\pl-ext.obj"
	-@erase "$(INTDIR)\pl-file.obj"
	-@erase "$(INTDIR)\pl-flag.obj"
	-@erase "$(INTDIR)\pl-fli.obj"
	-@erase "$(INTDIR)\pl-fmt.obj"
	-@erase "$(INTDIR)\pl-funct.obj"
	-@erase "$(INTDIR)\pl-gc.obj"
	-@erase "$(INTDIR)\pl-glob.obj"
	-@erase "$(INTDIR)\pl-itf.obj"
	-@erase "$(INTDIR)\pl-list.obj"
	-@erase "$(INTDIR)\pl-load.obj"
	-@erase "$(INTDIR)\pl-main.obj"
	-@erase "$(INTDIR)\pl-modul.obj"
	-@erase "$(INTDIR)\pl-nt.obj"
	-@erase "$(INTDIR)\pl-op.obj"
	-@erase "$(INTDIR)\pl-os.obj"
	-@erase "$(INTDIR)\pl-prims.obj"
	-@erase "$(INTDIR)\pl-pro.obj"
	-@erase "$(INTDIR)\pl-proc.obj"
	-@erase "$(INTDIR)\pl-prof.obj"
	-@erase "$(INTDIR)\pl-read.obj"
	-@erase "$(INTDIR)\pl-rec.obj"
	-@erase "$(INTDIR)\pl-save.obj"
	-@erase "$(INTDIR)\pl-setup.obj"
	-@erase "$(INTDIR)\pl-store.obj"
	-@erase "$(INTDIR)\pl-stream.obj"
	-@erase "$(INTDIR)\pl-sys.obj"
	-@erase "$(INTDIR)\pl-table.obj"
	-@erase "$(INTDIR)\pl-term.obj"
	-@erase "$(INTDIR)\pl-trace.obj"
	-@erase "$(INTDIR)\pl-util.obj"
	-@erase "$(INTDIR)\pl-wam.obj"
	-@erase "$(INTDIR)\pl-wic.obj"
	-@erase "$(INTDIR)\pl-write.obj"
	-@erase "$(OUTDIR)\libpl.dll"
	-@erase "$(OUTDIR)\libpl.exp"
	-@erase "$(OUTDIR)\libpl.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MT /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /Fp"$(INTDIR)/libpl.pch"\
 /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=d:\development\objects\pl/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libpl.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
LINK32_FLAGS=uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libpl.pdb" /machine:I386 /out:"$(OUTDIR)/libpl.dll"\
 /implib:"$(OUTDIR)/libpl.lib" 
LINK32_OBJS= \
	"$(INTDIR)\pl-arith.obj" \
	"$(INTDIR)\pl-atom.obj" \
	"$(INTDIR)\pl-bag.obj" \
	"$(INTDIR)\pl-buffer.obj" \
	"$(INTDIR)\pl-comp.obj" \
	"$(INTDIR)\pl-dde.obj" \
	"$(INTDIR)\pl-dll.obj" \
	"$(INTDIR)\pl-dump.obj" \
	"$(INTDIR)\pl-dwim.obj" \
	"$(INTDIR)\pl-ext.obj" \
	"$(INTDIR)\pl-file.obj" \
	"$(INTDIR)\pl-flag.obj" \
	"$(INTDIR)\pl-fli.obj" \
	"$(INTDIR)\pl-fmt.obj" \
	"$(INTDIR)\pl-funct.obj" \
	"$(INTDIR)\pl-gc.obj" \
	"$(INTDIR)\pl-glob.obj" \
	"$(INTDIR)\pl-itf.obj" \
	"$(INTDIR)\pl-list.obj" \
	"$(INTDIR)\pl-load.obj" \
	"$(INTDIR)\pl-main.obj" \
	"$(INTDIR)\pl-modul.obj" \
	"$(INTDIR)\pl-nt.obj" \
	"$(INTDIR)\pl-op.obj" \
	"$(INTDIR)\pl-os.obj" \
	"$(INTDIR)\pl-prims.obj" \
	"$(INTDIR)\pl-pro.obj" \
	"$(INTDIR)\pl-proc.obj" \
	"$(INTDIR)\pl-prof.obj" \
	"$(INTDIR)\pl-read.obj" \
	"$(INTDIR)\pl-rec.obj" \
	"$(INTDIR)\pl-save.obj" \
	"$(INTDIR)\pl-setup.obj" \
	"$(INTDIR)\pl-store.obj" \
	"$(INTDIR)\pl-stream.obj" \
	"$(INTDIR)\pl-sys.obj" \
	"$(INTDIR)\pl-table.obj" \
	"$(INTDIR)\pl-term.obj" \
	"$(INTDIR)\pl-trace.obj" \
	"$(INTDIR)\pl-util.obj" \
	"$(INTDIR)\pl-wam.obj" \
	"$(INTDIR)\pl-wic.obj" \
	"$(INTDIR)\pl-write.obj"

"$(OUTDIR)\libpl.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "WinDebug"
# PROP BASE Intermediate_Dir "WinDebug"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "d:\development\lib"
# PROP Intermediate_Dir "d:\development\objects\pl\debug"
OUTDIR=d:\development\lib
INTDIR=d:\development\objects\pl\debug

ALL : "$(OUTDIR)\libpl.dll"

CLEAN : 
	-@erase "$(INTDIR)\pl-arith.obj"
	-@erase "$(INTDIR)\pl-atom.obj"
	-@erase "$(INTDIR)\pl-bag.obj"
	-@erase "$(INTDIR)\pl-buffer.obj"
	-@erase "$(INTDIR)\pl-comp.obj"
	-@erase "$(INTDIR)\pl-dde.obj"
	-@erase "$(INTDIR)\pl-dll.obj"
	-@erase "$(INTDIR)\pl-dump.obj"
	-@erase "$(INTDIR)\pl-dwim.obj"
	-@erase "$(INTDIR)\pl-ext.obj"
	-@erase "$(INTDIR)\pl-file.obj"
	-@erase "$(INTDIR)\pl-flag.obj"
	-@erase "$(INTDIR)\pl-fli.obj"
	-@erase "$(INTDIR)\pl-fmt.obj"
	-@erase "$(INTDIR)\pl-funct.obj"
	-@erase "$(INTDIR)\pl-gc.obj"
	-@erase "$(INTDIR)\pl-glob.obj"
	-@erase "$(INTDIR)\pl-itf.obj"
	-@erase "$(INTDIR)\pl-list.obj"
	-@erase "$(INTDIR)\pl-load.obj"
	-@erase "$(INTDIR)\pl-main.obj"
	-@erase "$(INTDIR)\pl-modul.obj"
	-@erase "$(INTDIR)\pl-nt.obj"
	-@erase "$(INTDIR)\pl-op.obj"
	-@erase "$(INTDIR)\pl-os.obj"
	-@erase "$(INTDIR)\pl-prims.obj"
	-@erase "$(INTDIR)\pl-pro.obj"
	-@erase "$(INTDIR)\pl-proc.obj"
	-@erase "$(INTDIR)\pl-prof.obj"
	-@erase "$(INTDIR)\pl-read.obj"
	-@erase "$(INTDIR)\pl-rec.obj"
	-@erase "$(INTDIR)\pl-save.obj"
	-@erase "$(INTDIR)\pl-setup.obj"
	-@erase "$(INTDIR)\pl-store.obj"
	-@erase "$(INTDIR)\pl-stream.obj"
	-@erase "$(INTDIR)\pl-sys.obj"
	-@erase "$(INTDIR)\pl-table.obj"
	-@erase "$(INTDIR)\pl-term.obj"
	-@erase "$(INTDIR)\pl-trace.obj"
	-@erase "$(INTDIR)\pl-util.obj"
	-@erase "$(INTDIR)\pl-wam.obj"
	-@erase "$(INTDIR)\pl-wic.obj"
	-@erase "$(INTDIR)\pl-write.obj"
	-@erase "$(INTDIR)\vc40.idb"
	-@erase "$(INTDIR)\vc40.pdb"
	-@erase "$(OUTDIR)\libpl.dll"
	-@erase "$(OUTDIR)\libpl.exp"
	-@erase "$(OUTDIR)\libpl.ilk"
	-@erase "$(OUTDIR)\libpl.lib"
	-@erase "$(OUTDIR)\libpl.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
# ADD BASE CPP /nologo /MT /W3 /GX /Zi /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /FR /YX /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /Zi /Od /I "d:\development\pl\include" /D\
 "_DEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL"\
 /Fp"$(INTDIR)/libpl.pch" /YX /Fo"$(INTDIR)/" /Fd"$(INTDIR)/" /c 
CPP_OBJS=d:\development\objects\pl\debug/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "_DEBUG" /win32
# ADD MTL /nologo /D "_DEBUG" /win32
MTL_PROJ=/nologo /D "_DEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libpl.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
# ADD LINK32 uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386
LINK32_FLAGS=uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:yes\
 /pdb:"$(OUTDIR)/libpl.pdb" /debug /machine:I386 /out:"$(OUTDIR)/libpl.dll"\
 /implib:"$(OUTDIR)/libpl.lib" 
LINK32_OBJS= \
	"$(INTDIR)\pl-arith.obj" \
	"$(INTDIR)\pl-atom.obj" \
	"$(INTDIR)\pl-bag.obj" \
	"$(INTDIR)\pl-buffer.obj" \
	"$(INTDIR)\pl-comp.obj" \
	"$(INTDIR)\pl-dde.obj" \
	"$(INTDIR)\pl-dll.obj" \
	"$(INTDIR)\pl-dump.obj" \
	"$(INTDIR)\pl-dwim.obj" \
	"$(INTDIR)\pl-ext.obj" \
	"$(INTDIR)\pl-file.obj" \
	"$(INTDIR)\pl-flag.obj" \
	"$(INTDIR)\pl-fli.obj" \
	"$(INTDIR)\pl-fmt.obj" \
	"$(INTDIR)\pl-funct.obj" \
	"$(INTDIR)\pl-gc.obj" \
	"$(INTDIR)\pl-glob.obj" \
	"$(INTDIR)\pl-itf.obj" \
	"$(INTDIR)\pl-list.obj" \
	"$(INTDIR)\pl-load.obj" \
	"$(INTDIR)\pl-main.obj" \
	"$(INTDIR)\pl-modul.obj" \
	"$(INTDIR)\pl-nt.obj" \
	"$(INTDIR)\pl-op.obj" \
	"$(INTDIR)\pl-os.obj" \
	"$(INTDIR)\pl-prims.obj" \
	"$(INTDIR)\pl-pro.obj" \
	"$(INTDIR)\pl-proc.obj" \
	"$(INTDIR)\pl-prof.obj" \
	"$(INTDIR)\pl-read.obj" \
	"$(INTDIR)\pl-rec.obj" \
	"$(INTDIR)\pl-save.obj" \
	"$(INTDIR)\pl-setup.obj" \
	"$(INTDIR)\pl-store.obj" \
	"$(INTDIR)\pl-stream.obj" \
	"$(INTDIR)\pl-sys.obj" \
	"$(INTDIR)\pl-table.obj" \
	"$(INTDIR)\pl-term.obj" \
	"$(INTDIR)\pl-trace.obj" \
	"$(INTDIR)\pl-util.obj" \
	"$(INTDIR)\pl-wam.obj" \
	"$(INTDIR)\pl-wic.obj" \
	"$(INTDIR)\pl-write.obj"

"$(OUTDIR)\libpl.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Win32_Ru"
# PROP BASE Intermediate_Dir "Win32_Ru"
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "d:\development\plrt\bin"
# PROP Intermediate_Dir "d:\development\objects\pl\runtime"
OUTDIR=d:\development\plrt\bin
INTDIR=d:\development\objects\pl\runtime

ALL : "$(OUTDIR)\libpl.dll"

CLEAN : 
	-@erase "$(INTDIR)\pl-arith.obj"
	-@erase "$(INTDIR)\pl-atom.obj"
	-@erase "$(INTDIR)\pl-bag.obj"
	-@erase "$(INTDIR)\pl-buffer.obj"
	-@erase "$(INTDIR)\pl-comp.obj"
	-@erase "$(INTDIR)\pl-dde.obj"
	-@erase "$(INTDIR)\pl-dll.obj"
	-@erase "$(INTDIR)\pl-dump.obj"
	-@erase "$(INTDIR)\pl-dwim.obj"
	-@erase "$(INTDIR)\pl-ext.obj"
	-@erase "$(INTDIR)\pl-file.obj"
	-@erase "$(INTDIR)\pl-flag.obj"
	-@erase "$(INTDIR)\pl-fli.obj"
	-@erase "$(INTDIR)\pl-fmt.obj"
	-@erase "$(INTDIR)\pl-funct.obj"
	-@erase "$(INTDIR)\pl-gc.obj"
	-@erase "$(INTDIR)\pl-glob.obj"
	-@erase "$(INTDIR)\pl-itf.obj"
	-@erase "$(INTDIR)\pl-list.obj"
	-@erase "$(INTDIR)\pl-load.obj"
	-@erase "$(INTDIR)\pl-main.obj"
	-@erase "$(INTDIR)\pl-modul.obj"
	-@erase "$(INTDIR)\pl-nt.obj"
	-@erase "$(INTDIR)\pl-op.obj"
	-@erase "$(INTDIR)\pl-os.obj"
	-@erase "$(INTDIR)\pl-prims.obj"
	-@erase "$(INTDIR)\pl-pro.obj"
	-@erase "$(INTDIR)\pl-proc.obj"
	-@erase "$(INTDIR)\pl-prof.obj"
	-@erase "$(INTDIR)\pl-read.obj"
	-@erase "$(INTDIR)\pl-rec.obj"
	-@erase "$(INTDIR)\pl-save.obj"
	-@erase "$(INTDIR)\pl-setup.obj"
	-@erase "$(INTDIR)\pl-store.obj"
	-@erase "$(INTDIR)\pl-stream.obj"
	-@erase "$(INTDIR)\pl-sys.obj"
	-@erase "$(INTDIR)\pl-table.obj"
	-@erase "$(INTDIR)\pl-term.obj"
	-@erase "$(INTDIR)\pl-trace.obj"
	-@erase "$(INTDIR)\pl-util.obj"
	-@erase "$(INTDIR)\pl-wam.obj"
	-@erase "$(INTDIR)\pl-wic.obj"
	-@erase "$(INTDIR)\pl-write.obj"
	-@erase "$(OUTDIR)\libpl.dll"
	-@erase "$(OUTDIR)\libpl.exp"
	-@erase "$(OUTDIR)\libpl.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

"$(INTDIR)" :
    if not exist "$(INTDIR)/$(NULL)" mkdir "$(INTDIR)"

$(OUTDIR)/libpl.bsc : $(OUTDIR)  $(BSC32_SBRS)
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /I "c:\jan\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /YX /c
# SUBTRACT BASE CPP /Fr
# ADD CPP /nologo /MT /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /D "O_RUNTIME" /YX /c
# SUBTRACT CPP /Fr
CPP_PROJ=/nologo /MT /W3 /GX /O2 /I "d:\development\pl\include" /D "NDEBUG" /D\
 "WIN32" /D "_WINDOWS" /D "__WIN32__" /D "MAKE_PL_DLL" /D "O_RUNTIME"\
 /Fp"$(INTDIR)/libpl.pch" /YX /Fo"$(INTDIR)/" /c 
CPP_OBJS=d:\development\objects\pl\runtime/
CPP_SBRS=.\.
# ADD BASE MTL /nologo /D "NDEBUG" /win32
# ADD MTL /nologo /D "NDEBUG" /win32
MTL_PROJ=/nologo /D "NDEBUG" /win32 
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/libpl.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib c:\jan\pl\bin\uxnt.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
LINK32_FLAGS=uxnt.lib kernel32.lib user32.lib gdi32.lib winspool.lib\
 comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib\
 odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no\
 /pdb:"$(OUTDIR)/libpl.pdb" /machine:I386 /out:"$(OUTDIR)/libpl.dll"\
 /implib:"$(OUTDIR)/libpl.lib" 
LINK32_OBJS= \
	"$(INTDIR)\pl-arith.obj" \
	"$(INTDIR)\pl-atom.obj" \
	"$(INTDIR)\pl-bag.obj" \
	"$(INTDIR)\pl-buffer.obj" \
	"$(INTDIR)\pl-comp.obj" \
	"$(INTDIR)\pl-dde.obj" \
	"$(INTDIR)\pl-dll.obj" \
	"$(INTDIR)\pl-dump.obj" \
	"$(INTDIR)\pl-dwim.obj" \
	"$(INTDIR)\pl-ext.obj" \
	"$(INTDIR)\pl-file.obj" \
	"$(INTDIR)\pl-flag.obj" \
	"$(INTDIR)\pl-fli.obj" \
	"$(INTDIR)\pl-fmt.obj" \
	"$(INTDIR)\pl-funct.obj" \
	"$(INTDIR)\pl-gc.obj" \
	"$(INTDIR)\pl-glob.obj" \
	"$(INTDIR)\pl-itf.obj" \
	"$(INTDIR)\pl-list.obj" \
	"$(INTDIR)\pl-load.obj" \
	"$(INTDIR)\pl-main.obj" \
	"$(INTDIR)\pl-modul.obj" \
	"$(INTDIR)\pl-nt.obj" \
	"$(INTDIR)\pl-op.obj" \
	"$(INTDIR)\pl-os.obj" \
	"$(INTDIR)\pl-prims.obj" \
	"$(INTDIR)\pl-pro.obj" \
	"$(INTDIR)\pl-proc.obj" \
	"$(INTDIR)\pl-prof.obj" \
	"$(INTDIR)\pl-read.obj" \
	"$(INTDIR)\pl-rec.obj" \
	"$(INTDIR)\pl-save.obj" \
	"$(INTDIR)\pl-setup.obj" \
	"$(INTDIR)\pl-store.obj" \
	"$(INTDIR)\pl-stream.obj" \
	"$(INTDIR)\pl-sys.obj" \
	"$(INTDIR)\pl-table.obj" \
	"$(INTDIR)\pl-term.obj" \
	"$(INTDIR)\pl-trace.obj" \
	"$(INTDIR)\pl-util.obj" \
	"$(INTDIR)\pl-wam.obj" \
	"$(INTDIR)\pl-wic.obj" \
	"$(INTDIR)\pl-write.obj"

"$(OUTDIR)\libpl.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
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

.c{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cpp{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

.cxx{$(CPP_SBRS)}.sbr:
   $(CPP) $(CPP_PROJ) $<  

################################################################################
# Begin Target

# Name "libpl - Win32 Release"
# Name "libpl - Win32 Debug"
# Name "libpl - Win32 Runtime"

!IF  "$(CFG)" == "libpl - Win32 Release"

!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=".\pl-dwim.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_DW=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dwim.obj" : $(SOURCE) $(DEP_CPP_PL_DW) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_DW=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dwim.obj" : $(SOURCE) $(DEP_CPP_PL_DW) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_DW=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dwim.obj" : $(SOURCE) $(DEP_CPP_PL_DW) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-util.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_UT=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-util.obj" : $(SOURCE) $(DEP_CPP_PL_UT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_UT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-util.obj" : $(SOURCE) $(DEP_CPP_PL_UT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_UT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-util.obj" : $(SOURCE) $(DEP_CPP_PL_UT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-rec.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_RE=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-rec.obj" : $(SOURCE) $(DEP_CPP_PL_RE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_RE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-rec.obj" : $(SOURCE) $(DEP_CPP_PL_RE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_RE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-rec.obj" : $(SOURCE) $(DEP_CPP_PL_RE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-comp.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_CO=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-comp.obj" : $(SOURCE) $(DEP_CPP_PL_CO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_CO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-comp.obj" : $(SOURCE) $(DEP_CPP_PL_CO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_CO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-comp.obj" : $(SOURCE) $(DEP_CPP_PL_CO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-write.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_WR=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-write.obj" : $(SOURCE) $(DEP_CPP_PL_WR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_WR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-write.obj" : $(SOURCE) $(DEP_CPP_PL_WR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_WR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-write.obj" : $(SOURCE) $(DEP_CPP_PL_WR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-table.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_TA=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-table.obj" : $(SOURCE) $(DEP_CPP_PL_TA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_TA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-table.obj" : $(SOURCE) $(DEP_CPP_PL_TA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_TA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-table.obj" : $(SOURCE) $(DEP_CPP_PL_TA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-pro.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_PR=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-pro.obj" : $(SOURCE) $(DEP_CPP_PL_PR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_PR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-pro.obj" : $(SOURCE) $(DEP_CPP_PL_PR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_PR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-pro.obj" : $(SOURCE) $(DEP_CPP_PL_PR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-itf.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_IT=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-itf.obj" : $(SOURCE) $(DEP_CPP_PL_IT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_IT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-itf.obj" : $(SOURCE) $(DEP_CPP_PL_IT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_IT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-itf.obj" : $(SOURCE) $(DEP_CPP_PL_IT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-term.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_TE=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-term.obj" : $(SOURCE) $(DEP_CPP_PL_TE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_TE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-term.obj" : $(SOURCE) $(DEP_CPP_PL_TE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_TE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-term.obj" : $(SOURCE) $(DEP_CPP_PL_TE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-file.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_FI=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-file.obj" : $(SOURCE) $(DEP_CPP_PL_FI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_FI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-file.obj" : $(SOURCE) $(DEP_CPP_PL_FI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_FI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-file.obj" : $(SOURCE) $(DEP_CPP_PL_FI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-prims.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_PRI=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prims.obj" : $(SOURCE) $(DEP_CPP_PL_PRI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_PRI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prims.obj" : $(SOURCE) $(DEP_CPP_PL_PRI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_PRI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prims.obj" : $(SOURCE) $(DEP_CPP_PL_PRI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dde.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_DD=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dde.obj" : $(SOURCE) $(DEP_CPP_PL_DD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_DD=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dde.obj" : $(SOURCE) $(DEP_CPP_PL_DD) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_DD=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dde.obj" : $(SOURCE) $(DEP_CPP_PL_DD) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-stream.c"
DEP_CPP_PL_ST=\
	".\config.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

!IF  "$(CFG)" == "libpl - Win32 Release"


"$(INTDIR)\pl-stream.obj" : $(SOURCE) $(DEP_CPP_PL_ST) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"


"$(INTDIR)\pl-stream.obj" : $(SOURCE) $(DEP_CPP_PL_ST) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"


"$(INTDIR)\pl-stream.obj" : $(SOURCE) $(DEP_CPP_PL_ST) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-bag.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_BA=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-bag.obj" : $(SOURCE) $(DEP_CPP_PL_BA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_BA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-bag.obj" : $(SOURCE) $(DEP_CPP_PL_BA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_BA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-bag.obj" : $(SOURCE) $(DEP_CPP_PL_BA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-flag.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_FL=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-flag.obj" : $(SOURCE) $(DEP_CPP_PL_FL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_FL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-flag.obj" : $(SOURCE) $(DEP_CPP_PL_FL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_FL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-flag.obj" : $(SOURCE) $(DEP_CPP_PL_FL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-nt.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_NT=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-nt.obj" : $(SOURCE) $(DEP_CPP_PL_NT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_NT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-nt.obj" : $(SOURCE) $(DEP_CPP_PL_NT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_NT=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\console.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-nt.obj" : $(SOURCE) $(DEP_CPP_PL_NT) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-ext.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_EX=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-ext.obj" : $(SOURCE) $(DEP_CPP_PL_EX) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_EX=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-ext.obj" : $(SOURCE) $(DEP_CPP_PL_EX) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_EX=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-ext.obj" : $(SOURCE) $(DEP_CPP_PL_EX) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-prof.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_PRO=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prof.obj" : $(SOURCE) $(DEP_CPP_PL_PRO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_PRO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prof.obj" : $(SOURCE) $(DEP_CPP_PL_PRO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_PRO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-prof.obj" : $(SOURCE) $(DEP_CPP_PL_PRO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-wam.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_WA=\
	".\pl-alloc.c"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-index.c"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wam.obj" : $(SOURCE) $(DEP_CPP_PL_WA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_WA=\
	".\pl-alloc.c"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-index.c"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wam.obj" : $(SOURCE) $(DEP_CPP_PL_WA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_WA=\
	".\pl-alloc.c"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-index.c"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wam.obj" : $(SOURCE) $(DEP_CPP_PL_WA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-sys.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_SY=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-sys.obj" : $(SOURCE) $(DEP_CPP_PL_SY) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_SY=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-sys.obj" : $(SOURCE) $(DEP_CPP_PL_SY) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_SY=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-sys.obj" : $(SOURCE) $(DEP_CPP_PL_SY) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-main.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_MA=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-main.obj" : $(SOURCE) $(DEP_CPP_PL_MA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_MA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-main.obj" : $(SOURCE) $(DEP_CPP_PL_MA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_MA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-main.obj" : $(SOURCE) $(DEP_CPP_PL_MA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-load.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_LO=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-load.obj" : $(SOURCE) $(DEP_CPP_PL_LO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_LO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-load.obj" : $(SOURCE) $(DEP_CPP_PL_LO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_LO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-load.obj" : $(SOURCE) $(DEP_CPP_PL_LO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-os.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_OS=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-os.obj" : $(SOURCE) $(DEP_CPP_PL_OS) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_OS=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-os.obj" : $(SOURCE) $(DEP_CPP_PL_OS) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_OS=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-os.obj" : $(SOURCE) $(DEP_CPP_PL_OS) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-read.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_REA=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-read.obj" : $(SOURCE) $(DEP_CPP_PL_REA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_REA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-read.obj" : $(SOURCE) $(DEP_CPP_PL_REA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_REA=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-read.obj" : $(SOURCE) $(DEP_CPP_PL_REA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-glob.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_GL=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\dirent.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-glob.obj" : $(SOURCE) $(DEP_CPP_PL_GL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_GL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\dirent.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-glob.obj" : $(SOURCE) $(DEP_CPP_PL_GL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_GL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\dirent.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-glob.obj" : $(SOURCE) $(DEP_CPP_PL_GL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-modul.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_MO=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-modul.obj" : $(SOURCE) $(DEP_CPP_PL_MO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_MO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-modul.obj" : $(SOURCE) $(DEP_CPP_PL_MO) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_MO=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-modul.obj" : $(SOURCE) $(DEP_CPP_PL_MO) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-trace.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_TR=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-trace.obj" : $(SOURCE) $(DEP_CPP_PL_TR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_TR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-trace.obj" : $(SOURCE) $(DEP_CPP_PL_TR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_TR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-trace.obj" : $(SOURCE) $(DEP_CPP_PL_TR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-store.c"

!IF  "$(CFG)" == "libpl - Win32 Release"


"$(INTDIR)\pl-store.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"


"$(INTDIR)\pl-store.obj" : $(SOURCE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"


"$(INTDIR)\pl-store.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-setup.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_SE=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-setup.obj" : $(SOURCE) $(DEP_CPP_PL_SE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_SE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-setup.obj" : $(SOURCE) $(DEP_CPP_PL_SE) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_SE=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-setup.obj" : $(SOURCE) $(DEP_CPP_PL_SE) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-save.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_SA=\
	".\morecore.c"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-save.obj" : $(SOURCE) $(DEP_CPP_PL_SA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_SA=\
	".\morecore.c"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-save.obj" : $(SOURCE) $(DEP_CPP_PL_SA) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_SA=\
	".\morecore.c"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-save.obj" : $(SOURCE) $(DEP_CPP_PL_SA) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-proc.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_PROC=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-proc.obj" : $(SOURCE) $(DEP_CPP_PL_PROC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_PROC=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-proc.obj" : $(SOURCE) $(DEP_CPP_PL_PROC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_PROC=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-proc.obj" : $(SOURCE) $(DEP_CPP_PL_PROC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dll.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_DL=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dll.obj" : $(SOURCE) $(DEP_CPP_PL_DL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_DL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dll.obj" : $(SOURCE) $(DEP_CPP_PL_DL) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_DL=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-dll.obj" : $(SOURCE) $(DEP_CPP_PL_DL) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-dump.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_DU=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_PL_DU=\
	".\gnu\unexec.c"\
	

"$(INTDIR)\pl-dump.obj" : $(SOURCE) $(DEP_CPP_PL_DU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_DU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_PL_DU=\
	".\gnu\unexec.c"\
	

"$(INTDIR)\pl-dump.obj" : $(SOURCE) $(DEP_CPP_PL_DU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_DU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-save.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	
NODEP_CPP_PL_DU=\
	".\gnu\unexec.c"\
	

"$(INTDIR)\pl-dump.obj" : $(SOURCE) $(DEP_CPP_PL_DU) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-op.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_OP=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-op.obj" : $(SOURCE) $(DEP_CPP_PL_OP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_OP=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-op.obj" : $(SOURCE) $(DEP_CPP_PL_OP) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_OP=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-op.obj" : $(SOURCE) $(DEP_CPP_PL_OP) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-funct.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_FU=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funct.ic"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-funct.obj" : $(SOURCE) $(DEP_CPP_PL_FU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_FU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ic"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-funct.obj" : $(SOURCE) $(DEP_CPP_PL_FU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_FU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ic"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-funct.obj" : $(SOURCE) $(DEP_CPP_PL_FU) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-list.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_LI=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-list.obj" : $(SOURCE) $(DEP_CPP_PL_LI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_LI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-list.obj" : $(SOURCE) $(DEP_CPP_PL_LI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_LI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-list.obj" : $(SOURCE) $(DEP_CPP_PL_LI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-fmt.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_FM=\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fmt.obj" : $(SOURCE) $(DEP_CPP_PL_FM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_FM=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fmt.obj" : $(SOURCE) $(DEP_CPP_PL_FM) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_FM=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fmt.obj" : $(SOURCE) $(DEP_CPP_PL_FM) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-arith.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_AR=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-arith.obj" : $(SOURCE) $(DEP_CPP_PL_AR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_AR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-arith.obj" : $(SOURCE) $(DEP_CPP_PL_AR) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_AR=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-arith.obj" : $(SOURCE) $(DEP_CPP_PL_AR) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-fli.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_FLI=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fli.obj" : $(SOURCE) $(DEP_CPP_PL_FLI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_FLI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fli.obj" : $(SOURCE) $(DEP_CPP_PL_FLI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_FLI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-fli.obj" : $(SOURCE) $(DEP_CPP_PL_FLI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-wic.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_WI=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wic.obj" : $(SOURCE) $(DEP_CPP_PL_WI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_WI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wic.obj" : $(SOURCE) $(DEP_CPP_PL_WI) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_WI=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-wic.obj" : $(SOURCE) $(DEP_CPP_PL_WI) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-buffer.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_BU=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-buffer.obj" : $(SOURCE) $(DEP_CPP_PL_BU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_BU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-buffer.obj" : $(SOURCE) $(DEP_CPP_PL_BU) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_BU=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-buffer.obj" : $(SOURCE) $(DEP_CPP_PL_BU) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-gc.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_GC=\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-gc.obj" : $(SOURCE) $(DEP_CPP_PL_GC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_GC=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-gc.obj" : $(SOURCE) $(DEP_CPP_PL_GC) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_GC=\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-gc.obj" : $(SOURCE) $(DEP_CPP_PL_GC) "$(INTDIR)"


!ENDIF 

# End Source File
################################################################################
# Begin Source File

SOURCE=".\pl-atom.c"

!IF  "$(CFG)" == "libpl - Win32 Release"

DEP_CPP_PL_AT=\
	".\pl-atom.ic"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-atom.obj" : $(SOURCE) $(DEP_CPP_PL_AT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Debug"

DEP_CPP_PL_AT=\
	".\pl-atom.ic"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-atom.obj" : $(SOURCE) $(DEP_CPP_PL_AT) "$(INTDIR)"


!ELSEIF  "$(CFG)" == "libpl - Win32 Runtime"

DEP_CPP_PL_AT=\
	".\pl-atom.ic"\
	".\pl-atom.ih"\
	".\pl-buffer.h"\
	".\pl-ctype.h"\
	".\pl-data.h"\
	".\pl-funcs.h"\
	".\pl-funct.ih"\
	".\pl-incl.h"\
	".\pl-itf.h"\
	".\pl-main.h"\
	".\pl-os.h"\
	".\pl-stream.h"\
	"d:\development\pl\include\uxnt.h"\
	{$(INCLUDE)}"\sys\STAT.H"\
	{$(INCLUDE)}"\sys\TYPES.H"\
	

"$(INTDIR)\pl-atom.obj" : $(SOURCE) $(DEP_CPP_PL_AT) "$(INTDIR)"


!ENDIF 

# End Source File
# End Target
# End Project
################################################################################
