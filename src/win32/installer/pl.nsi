# SWI-Prolog install-script

!define TEMP1 $R0 ; Temp variable
!define EXT    $3 ; Filename extension for Prolog sources
!define CWD    $4 ; Working directory for startmenu shortcut
!define GRP    $5 ; Startmenu group
!define DEFCWD $6 ; Default working directory
!define SHCTX  $7 ; Shell context (current/all)

!system "pl\bin\plcon.exe -f mkinstaller.pl -g true -t main" = 0
!include "version.nsi"

SetCompressor bzip2
MiscButtonText "<back" "next>" "abort" "finished"

# Preload files that are needed by the installer itself
ReserveFile "${NSISDIR}\Plugins\UserInfo.dll"
ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
ReserveFile "options.ini"

InstallDir $PROGRAMFILES\pl
InstallDirRegKey HKLM SOFTWARE\SWI\Prolog "home"
ComponentText "This will install the SWI-Prolog on your computer. \
               Select which optional components you want installed."
DirText "This program will install SWI-Prolog on your computer.\
         Choose a directory"

LicenseData pl\COPYING.TXT
LicenseText "SWI-Prolog is governed by the LGPL"

Page license
Page components
Page directory
Page custom SetCustom ": Installation options"
Page instfiles

Section "Base system (required)"
  SectionIn RO			# do not allow to delete this

  SetOutPath $INSTDIR\bin
  File pl\bin\plcon.exe
  File pl\bin\plwin.exe
  File pl\bin\libpl.dll
  File pl\bin\plterm.dll
  File pl\bin\plregtry.dll

  File pl\bin\pthreadVC.dll
  SetOutPath $INSTDIR
  File /r pl\custom
  File pl\boot32.prc
  File pl\COPYING.TXT
  File pl\README.TXT
  File pl\READWIN.TXT
  File pl\VERSION
  File pl\swipl

  SetOutPath $INSTDIR\library
; SYSTEM STUFF
  File pl\library\listing.pl
  File pl\library\qsave.pl
  File pl\library\statistics.pl
  File pl\library\shlib.pl
  File pl\library\system.pl
  File pl\library\threadutil.pl
  File pl\library\tty.pl

; COMPATIBILITY
  File pl\library\backcomp.pl
  File pl\library\bim.pl
  File pl\library\edinburgh.pl
  File pl\library\qpforeign.pl
  File pl\library\quintus.pl
  File pl\library\files.pl

; `STANDARD LIBRARIES'
  File pl\library\ctypes.pl
  File pl\library\gensym.pl
  File pl\library\lists.pl
  File pl\library\occurs.pl
  File pl\library\ordsets.pl
  File pl\library\oset.pl

; WINDOWS
  File pl\library\dde.pl
  File pl\library\progman.pl
  File pl\library\registry.pl
  File pl\library\win_menu.pl
  File pl\library\wise.pl

; DEVELOPMENT
  File pl\library\edit.pl
  File pl\library\make.pl
  File pl\library\emacs_interface.pl
  File pl\library\explain.pl
  File pl\library\debug.pl
  File pl\library\check.pl
  File pl\library\checklast.pl
  File pl\library\checkselect.pl
  File pl\library\shell.pl

; WEB STUFF
  File pl\library\netscape.pl
  File pl\library\url.pl

; MISC
  File pl\library\am_match.pl
  File pl\library\readln.pl
  File pl\library\readutil.pl
  File pl\library\streampool.pl

  SetOutPath $INSTDIR\doc
  File pl\doc\windows.html
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\index.html

  WriteRegStr HKLM SOFTWARE\SWI\Prolog "home" "$INSTDIR"

  ; Write uninstaller
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog" "DisplayName" "SWI-Prolog (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "Documentation and Help-system"
  SetOutPath $INSTDIR
  File pl\ChangeLog.TXT
  SetOutPath $INSTDIR\library
  File pl\library\MANUAL
  File pl\library\helpidx.pl
  File pl\library\help.pl
SectionEnd

Section "Demo files"
  SetOutPath $INSTDIR
  File /r pl\demo
  SetOutPath $INSTDIR\bin
  File pl\bin\dlltest.dll
SectionEnd

Section "C/C++ Interface"
  SetOutPath $INSTDIR
  File /r pl\lib
  File /r pl\include
  SetOutPath $INSTDIR\bin
  File pl\bin\plld.exe
  File pl\bin\plrc.exe
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\pl2cpp.html
SectionEnd

Section "XPCE graphics library"
  SetOutPath $INSTDIR
  File /r pl\xpce
  File pl\plwin.rc
  SetOutPath $INSTDIR\bin
  File pl\bin\pl2xpce.dll
  File pl\bin\xpce-stub.exe
SectionEnd

Section "Package CLIB"
  SetOutPath $INSTDIR\bin
  File pl\bin\cgi.dll
  File pl\bin\memfile.dll
  File pl\bin\mime.dll
  File pl\bin\socket.dll
  File pl\bin\time.dll
  SetOutPath $INSTDIR\library
  File pl\library\cgi.pl
  File pl\library\memfile.pl
  File pl\library\mime.pl
  File pl\library\socket.pl
  File pl\library\time.pl
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\clib.html
SectionEnd

Section "ODBC Interface"
  SetOutPath $INSTDIR\bin
  File pl\bin\odbc4pl.dll
  SetOutPath $INSTDIR\library
  File pl\library\odbc.pl
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\odbc.html
SectionEnd

Section "SGML/XML/HTML parser"
  SetOutPath $INSTDIR\bin
  File pl\bin\sgml2pl.dll
  SetOutPath $INSTDIR\library
  File /r pl\library\DTD
  File pl\library\sgml.pl
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\sgml2pl.html
SectionEnd

Section "RDF and Semantic Web Library"
  SetOutPath $INSTDIR\bin
  File pl\bin\rdf_db.dll

  SetOutPath $INSTDIR\library
  File pl\library\rdf.pl
  File pl\library\rdf_parser.pl
  File pl\library\rdf_triple.pl
  File pl\library\rewrite.pl
  File /r pl\library\semweb

  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\rdf2pl.html
  File pl\doc\packages\semweb.html
  File pl\doc\packages\modules.gif
SectionEnd

Section "HTTP Client/Server package"
  SetOutPath $INSTDIR\library
  File /r pl\library\http
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\http.html
  File pl\doc\packages\httpserver.gif
  SetOutPath $INSTDIR\doc\packages\examples
  File /r pl\doc\packages\examples\http
SectionEnd

Section "Table package"
  SetOutPath $INSTDIR\bin
  File pl\bin\table.dll
  SetOutPath $INSTDIR\library
  File pl\library\table.pl
  File pl\library\table_util.pl
  SetOutPath $INSTDIR\doc\packages
  File pl\doc\packages\table.html
SectionEnd

Section "Sources for system predicates"
  SetOutPath $INSTDIR
  File /r pl\boot
  SetOutPath $INSTDIR\bin
  File pl\bin\mkboot.bat
SectionEnd

Section "Shell Extensions" SecShell
; SectionIn 1 2 3
  ; back up old value of .pl
  ReadRegStr $1 HKCR .${EXT} ""
  StrCmp $1 "" Label1
    StrCmp $1 "PrologFile" Label1
    WriteRegStr HKCR .${EXT} "backup_val" $1
  Label1:
  WriteRegStr HKCR .${EXT} "" "PrologFile"

  ReadRegStr $0 HKCR "PrologFile" ""
  StrCmp $0 "" 0 skipNSIAssoc
	WriteRegStr HKCR "PrologFile" "" "Prolog Source"
	WriteRegStr HKCR "PrologFile\shell" "" "open"
	WriteRegStr HKCR "PrologFile\DefaultIcon" "" $INSTDIR\bin\plwin.exe,0
  skipNSIAssoc:
  ; OPEN
  WriteRegStr HKCR "PrologFile\shell\open\command" "" '"$INSTDIR\bin\plwin.exe" "%1"'
!ifdef SHELL_DDE
  ; EDIT (these are not yet correct)
  WriteRegStr HKCR "PrologFile\shell\edit" "" "Edit Prolog Source"
  WriteRegStr HKCR "PrologFile\shell\edit\command" "" '"$INSTDIR\bin\plwin.exe'
  WriteRegStr HKCR "PrologFile\shell\edit\ddeexec" "" "edit('%1')"
  WriteRegStr HKCR "PrologFile\shell\edit\ddeexec" "Application" "prolog"
  WriteRegStr HKCR "PrologFile\shell\edit\ddeexec" "ifexec" ""
  WriteRegStr HKCR "PrologFile\shell\edit\ddeexec" "Topic" "control"
  ; CONSULT
  WriteRegStr HKCR "PrologFile\shell\consult" "" "Load Prolog Source"
  WriteRegStr HKCR "PrologFile\shell\consult\command" "" '"$INSTDIR\bin\plwin.exe'
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "" "consult('%1')"
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "Application" "prolog"
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "ifexec" ""
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "Topic" "control"
!endif

  WriteRegStr HKLM SOFTWARE\SWI\Prolog fileExtension ${EXT}
SectionEnd

!macro Create_Internet_Shorcut URLName URLhost
  FileOpen $0 "$INSTDIR\doc\${URLName}.url" w
  FileWrite $0 "[InternetShortcut]$\r$\n"
  FileWrite $0 "URL=${URLhost}"
  FileClose $0
  CreateShortCut "$SMPROGRAMS\${GRP}\${URLName}.lnk" \
		 "$INSTDIR\doc\${URLName}.url" "" \
		 "$INSTDIR\doc\${URLName}.url" \
		 0 "SW_SHOWNORMAL" "" "Visit the Web site"
!macroend

Section "Start Menu shortcuts"
  SetOutPath ${CWD}
  CreateDirectory "$SMPROGRAMS\${GRP}"
  IfFileExists "$SMPROGRAMS\${GRP}\XPCE.lnk" 0 NoOldXPCE
    Delete "$SMPROGRAMS\${GRP}\XPCE.lnk"
  NoOldXPCE:
  CreateShortCut "$SMPROGRAMS\${GRP}\Prolog.lnk" \
		 "$INSTDIR\bin\plwin.exe" \
		 "" \
		 "$INSTDIR\bin\plwin.exe" \
		 0
  SetOutPath $INSTDIR
  CreateShortCut "$SMPROGRAMS\${GRP}\Readme.lnk" \
  		  "$INSTDIR\doc\windows.html" "" \
		  "$INSTDIR\doc\windows.html" 0 \
		  "SW_SHOWNORMAL" "" "View readme"
  !insertmacro Create_Internet_Shorcut "SWI-Prolog website" \
		 "http://www.swi-prolog.org"
  !insertmacro Create_Internet_Shorcut "Support SWI-Prolog development" \
		 "http://www.swi-prolog.org/donate.html"
  CreateShortCut "$SMPROGRAMS\${GRP}\Uninstall.lnk" \
		 "$INSTDIR\uninstall.exe" \
		 "" \
		 "$INSTDIR\uninstall.exe" \
		 0

  WriteRegStr HKLM SOFTWARE\SWI\Prolog group   ${GRP}
  WriteRegStr HKLM SOFTWARE\SWI\Prolog cwd     ${CWD}
  WriteRegStr HKLM SOFTWARE\SWI\Prolog context ${SHCTX}
SectionEnd

Section "Update library index"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\plwin.exe" -f none -g "make_library_index(swi(library)),halt"'
SectionEnd

Section "Precompiled libraries"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\plwin.exe" -f none -g wise_install_xpce,halt'
SectionEnd

################################################################
# The uninstaller
################################################################

UninstallText "This will uninstall SWI-Prolog. Hit Uninstall to continue."

Section "Uninstall"
  ReadRegStr ${EXT}   HKLM Software\SWI\Prolog fileExtension
  ReadRegStr ${GRP}   HKLM Software\SWI\Prolog group
  ReadRegStr ${SHCTX} HKLM Software\SWI\Prolog context

  StrCmp ${SHCTX} "all" 0 +2
    SetShellVarContext all

  MessageBox MB_YESNO "Delete the following components?$\r$\n \
                       Install dir: $INSTDIR$\r$\n \
		       Extension: ${EXT}$\r$\n \
		       Program Group ${GRP}" \
		      IDNO Done

  StrCmp ".${EXT}" "" NoExt
    ReadRegStr $1 HKCR .${EXT} ""
    StrCmp $1 "PrologFile" 0 NoOwn ; only do this if we own it
      ReadRegStr $1 HKCR .${EXT} "backup_val"
      StrCmp $1 "" 0 RestoreBackup ; if backup == "" then delete the whole key
	DeleteRegKey HKCR .${EXT}
      Goto NoOwn
      RestoreBackup:
	WriteRegStr HKCR .${EXT} "" $1
	DeleteRegValue HKCR .${EXT} "backup_val"
    NoOwn:
  NoExt:

  StrCmp "${GRP}" "" NoGrp
    MessageBox MB_OK "Deleting $SMPROGRAMS\${GRP}"
    RMDir /r "$SMPROGRAMS\${GRP}"
  NoGrp:

  IfFileExists "$INSTDIR\bin\plwin.exe" 0 NoDir
    RMDir /r "$INSTDIR"
    goto Done

  NoDir:
    MessageBox MB_OK "Folder $INSTDIR doesn't seem to contain Prolog"

  Done:
SectionEnd

################################################################
# FUNCTIONS
################################################################

Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes
  
  InitPluginsDir
  File /oname=$PLUGINSDIR\options.ini "options.ini"

FunctionEnd

################################################################
# Handle customisation;  Settings are maintained in
#
# 	HKLM SOFTWARE\SWI\Prolog
#
# Using the following mapping:
#
#	${EXT} fileExtension
################################################################

Function SetCustom
# Basic system info
  Call UserInfo

# Filename extension
  ReadRegStr ${EXT} HKLM SOFTWARE\SWI\Prolog fileExtension
  StrCmp ${EXT} "" 0 HasExt
    StrCpy ${EXT} "pl"
  HasExt:
  WriteINIStr $PLUGINSDIR\options.ini "Field 4" "State" ${EXT}  

# Startmenu program group
  ReadRegStr ${GRP} HKLM SOFTWARE\SWI\Prolog group
  StrCmp ${GRP} "" 0 HasGroup
    StrCpy ${GRP} "SWI-Prolog"
  HasGroup:
  WriteINIStr $PLUGINSDIR\options.ini "Field 6" "State" ${GRP}  

# Working Directory
  ReadRegStr ${CWD} HKLM SOFTWARE\SWI\Prolog cwd
  StrCmp ${CWD} "" 0 HasCWD
    StrCpy ${CWD} ${DEFCWD}
  HasCWD:
  WriteINIStr $PLUGINSDIR\options.ini "Field 8" "State" ${CWD}

# Start the dialog
  Push ${TEMP1}
  InstallOptions::dialog "$PLUGINSDIR\options.ini"
  Pop ${TEMP1}
  Pop ${TEMP1}

# Get the results
  ReadINIStr ${EXT} $PLUGINSDIR\options.ini "Field 4" "State"
  ReadINIStr ${GRP} $PLUGINSDIR\options.ini "Field 6" "State"
  ReadINIStr ${CWD} $PLUGINSDIR\options.ini "Field 8" "State"
FunctionEnd

Function UserInfo
  ClearErrors
  UserInfo::GetName
  IfErrors Win9x
  Pop $0
  UserInfo::GetAccountType
  Pop $1

  StrCmp $1 "Admin" 0 +4
    SetShellVarContext all
    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "Power" 0 +3
    StrCpy ${SHCTX} "all"
    Goto done
  StrCmp $1 "User" 0 +3
    StrCpy ${SHCTX} "current"
    Goto done
  StrCmp $1 "Guest" 0 +3
    StrCpy ${SHCTX} "current"
    Goto done
  StrCpy ${SHCTX} "current"		# Unkown accounttype
    Goto done

  Win9x:
    StrCpy ${DEFCWD} $INSTDIR\demo
    StrCpy ${SHCTX}  "current"
    Goto end

  done:
    StrCmp ${SHCTX} "all" 0 +2
      SetShellVarContext all
    StrCpy ${DEFCWD} $DESKTOP\Prolog

  end:
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Installation complete. View readme?" IDNO NoReadme
  ExecShell "open" "$INSTDIR\doc\windows.html"
  NoReadme:
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact prolog-bugs@swi.psy.uva.nl"
FunctionEnd
