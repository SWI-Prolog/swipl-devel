# SWI-Prolog install-script

!define TEMP1 $R0 ; Temp variable
!define EXT    $3 ; Filename extension for Prolog sources
!define CWD    $4 ; Working directory for startmenu shortcut
!define GRP    $5 ; Startmenu group
!define SHCTX  $6 ; Shell context (current/all)
!define ARCH   $7 ; Architecture (x86, ia64 or amd64)
!define SXSLEN $8 ; The length of the string of the location of the SideBySide directory
Var /GLOBAL cmdLineParams  ; Command Line Options

!define REGKEY SOFTWARE\SWI\Prolog

!ifdef MINGW
!system "${SWIPL}\bin\swipl.exe -f mkinstaller.pl -g true -t main -- /DSWIPL=${SWIPL} /DPTHREAD=${PTHREAD} /DZLIB=${ZLIB} /DBOOT=${BOOT} /DMINGW=1" = 0
!else
!system "${SWIPL}\bin\swipl.exe -f mkinstaller.pl -g true -t main -- /DSWIPL=${SWIPL} /DPTHREAD=${PTHREAD} /DZLIB=${ZLIB} /DBOOT=${BOOT}" = 0
!endif
!include "version.nsi"
!include "FileFunc.nsh"

RequestExecutionLevel admin
SetCompressor bzip2
MiscButtonText "<back" "next>" "abort" "finished"

# Preload files that are needed by the installer itself
ReserveFile "${NSISDIR}\Plugins\UserInfo.dll"
ReserveFile "${NSISDIR}\Plugins\InstallOptions.dll"
ReserveFile "options.ini"

!ifdef WIN64
!define BITS 64
InstallDir $PROGRAMFILES64\swipl
!else
!define BITS 32
InstallDir $PROGRAMFILES\swipl
!endif

ComponentText "This will install the SWI-Prolog on your computer. \
               Select which optional components you want installed."
DirText "This program will install SWI-Prolog on your computer.\
         Choose a directory"

VIProductVersion "${_VERSION}"
VIAddVersionKey Comments "SWI-Prolog installer for Windows"
VIAddVersionKey ProductName "SWI-Prolog"
VIAddVersionKey ProductVersion "${_VERSION}"
VIAddVersionKey CompanyName "swi-prolog.org"
VIAddVersionKey LegalCopyright "LGPL"
VIAddVersionKey FileVersion "${_VERSION}"
VIAddVersionKey OriginalFilename "${_OUTFILE}"

Icon ${SWIPL}\swipl.ico
LicenseData ${SWIPL}\COPYING.TXT
LicenseText "SWI-Prolog is governed by the LGPL"

!ifdef MINGW
InstType "Typical"				# 1
!else
InstType "Typical (all except debug symbols)"	# 1
!endif
InstType "Minimal (no graphics)"		# 2
InstType "Full"					# 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Make sure we have the VC8 runtime environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

!ifdef WIN64
!define MACHTYPE "amd"
!define REDISTFILE "vcredist_x64.exe"
!define VCRT_URL "http://download.microsoft.com/download/d/2/4/d242c3fb-da5a-4542-ad66-f9661d0a8d19/vcredist_x64.exe"
!else
!define MACHTYPE "x86"
!define REDISTFILE "vcredist_x32.exe"
!define VCRT_URL "http://download.microsoft.com/download/d/d/9/dd9a82d0-52ef-40db-8dab-795376989c03/vcredist_x86.exe"
!endif


!macro CallFindFiles DIR FILE CBFUNC
Push "${DIR}"
Push "${FILE}"
Push $0
GetFunctionAddress $0 "${CBFUNC}"
Exch $0
Call FindFiles
!macroend

!ifndef MINGW
Section "Microsoft VC runtime libraries"
  SectionIn 1 2 3
  ; Only checking the Windows Side-by-Side folder for occurences of mcvcr90.dll
  ; Change msvcr90.dll into something non-existen to force download for testing
  ; purposes.
  ; Set length of the windows side by side string length.
  StrLen ${SXSLEN} "$WINDIR\WinSxS\"
  !insertmacro CallFindFiles "$WINDIR\WinSxS" msvcr90.dll FindVCRT
  ; have to check again, now to deteremine to launch the downloader (or not)...
  StrCmp $0 ${MACHTYPE} found not_found
    found:
    Return
    not_found:
    ; for debug
    ; MessageBox MB_OK "Couldn't find msvcr_90.dll"
    call GetVCRT
SectionEnd


Function GetVCRT
        MessageBox MB_YESNO "Microsoft Visual C++ 2008 SP1 Redistributable will now be$\r$\n\
                             downloaded and installed.$\r$\n$\n\
                             Administrative rights might be required! Do you want \
                             to continue?"\
                   IDYES download_install IDNO abort_install

        download_install:
        StrCpy $2 "$TEMP\${REDISTFILE}"
        nsisdl::download /TIMEOUT=30000 ${VCRT_URL} $2
        Pop $R0 ;Get the return value
                StrCmp $R0 "success" +3
                MessageBox MB_OK "Download failed: $R0"
                Quit
        ClearErrors
        ExecWait "$2 /q"
        IfErrors failure dl_ok

        failure:
        MessageBox MB_OK "An error has occured, Microsoft Visual C++ 2008 SP1 \
                          Redistributable$\r$\n\
                          has not been installed"
        goto abort_install

        dl_ok:
        MessageBox MB_YESNO "Microsoft Visual C++ 2008 SP1 Redistributable$\r$\n\
                             has been installed successfully to your system,$\r$\n\
                             in order to finalise the installation, a reboot is \
                             required.$\r$\n$\n\
                             Would you like to reboot now?"\
                   IDYES re_boot IDNO abort_install

        re_boot:
        MessageBox MB_OK "After your system has rebooted, you will have to re-start the$\r$\n\
                          the SWI-Prolog installation process by clicking on the installer."
        Delete $2
        Reboot
        Return

        abort_install:
        Abort "Installation has been interupted"
FunctionEnd


Function FindVCRT
  Pop $0

  ; Checking for the first 3 characters of the WinSxS sub-dirs, they start with
  ; either amd64_ or x86_, so first get those 3 characters:
  StrCpy $0 $0 3 ${SXSLEN}
  ; and then compare
  StrCmp $0 ${MACHTYPE} found not_found

  found:
  ; set the stop criterium
  Push "stop"
  Return

  not_found:
  ; avoid stack corruption
  Push "continue"
FunctionEnd

; Function taken from here: http://nsis.sourceforge.net/Search_For_a_File

Function FindFiles
  Exch $R5 # callback function
  Exch
  Exch $R4 # file name
  Exch 2
  Exch $R0 # directory
  Push $R1
  Push $R2
  Push $R3
  Push $R6

  Push $R0 # first dir to search

  StrCpy $R3 1

  nextDir:
    Pop $R0
    IntOp $R3 $R3 - 1
    ClearErrors
    FindFirst $R1 $R2 "$R0\*.*"
    nextFile:
      StrCmp $R2 "." gotoNextFile
      StrCmp $R2 ".." gotoNextFile

      StrCmp $R2 $R4 0 isDir
        Push "$R0\$R2"
        Call $R5
        Pop $R6
        StrCmp $R6 "stop" 0 isDir
          loop:
            StrCmp $R3 0 done
            Pop $R0
            IntOp $R3 $R3 - 1
            Goto loop

      isDir:
        IfFileExists "$R0\$R2\*.*" 0 gotoNextFile
          IntOp $R3 $R3 + 1
          Push "$R0\$R2"

  gotoNextFile:
    FindNext $R1 $R2
    IfErrors 0 nextFile

  done:
    FindClose $R1
    StrCmp $R3 0 0 nextDir

  Pop $R6
  Pop $R3
  Pop $R2
  Pop $R1
  Pop $R0
  Pop $R5
  Pop $R4
FunctionEnd
!endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; End MSVCRT check/install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Section "-Silent"
  SectionIn RO
  # A hidden section for reading in command line options
  # And setting default values

  IfSilent 0 notsilent
    StrCpy ${EXT} "pl"
    StrCpy ${GRP} "SWI-Prolog"
    Call UserInfo

  notsilent:
  ${GetOptions} $cmdLineParams "/EXT=" $R0
  IfErrors +2 0
  StrCpy ${EXT} $R0

  ${GetOptions} $cmdLineParams "/GRP=" $R0
  IfErrors +2 0
  StrCpy ${GRP} $R0

  ${GetOptions} $cmdLineParams "/INSTDIR=" $R0
  IfErrors +2 0
  StrCpy $INSTDIR $R0
SectionEnd

!ifdef WIN64
Page custom Check64 "" ": Checking for AMD64 architecture"
!endif
Page license
Page components
Page directory
Page custom SetCustom "" ": Installation options"
Page instfiles

Section "Base system (required)"
  SectionIn RO			# do not allow to delete this

  Delete $INSTDIR\bin\*.pdb
  RmDir /r pl\custom		# old location of pl\customize

  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\swipl.exe
  File ${SWIPL}\bin\swipl-win.exe
  File ${SWIPL}\bin\plterm.dll
  File ${SWIPL}\bin\plregtry.dll
  File ${SWIPL}\bin\${PTHREAD}.dll
!ifdef MINGW
  File ${SWIPL}\bin\libswipl.dll
  File ${SWIPL}\bin\libeay32.dll
  File ${SWIPL}\bin\libgmp-10.dll
  File ${SWIPL}\bin\libjpeg-8.dll
  File ${SWIPL}\bin\ssleay32.dll
  File ${SWIPL}\bin\libarchive-13.dll
  File /nonfatal ${SWIPL}\bin\libdwarf.dll
  File /nonfatal ${SWIPL}\bin\libgcc_s_sjlj-1.dll
!else
  File ${SWIPL}\bin\swipl.dll
!endif

  SetOutPath $INSTDIR
  File /r ${SWIPL}\customize
  File ${SWIPL}\${BOOT}
  File ${SWIPL}\COPYING.TXT
  File ${SWIPL}\README.TXT
  File ${SWIPL}\VERSION
  File ${SWIPL}\swipl.home
  File ${SWIPL}\swipl.ico

  SetOutPath $INSTDIR\library
; SYSTEM STUFF
  File ${SWIPL}\library\listing.pl
  File ${SWIPL}\library\pprint.pl
  File ${SWIPL}\library\qsave.pl
  File ${SWIPL}\library\statistics.pl
  File ${SWIPL}\library\writef.pl
  File ${SWIPL}\library\shlib.pl
  File ${SWIPL}\library\system.pl
  File ${SWIPL}\library\threadutil.pl
  File ${SWIPL}\library\thread.pl
  File ${SWIPL}\library\thread_pool.pl
  File ${SWIPL}\library\tty.pl
  File ${SWIPL}\library\dif.pl
  File ${SWIPL}\library\when.pl
  File ${SWIPL}\library\varnumbers.pl
  File ${SWIPL}\library\prolog_stack.pl
  File ${SWIPL}\library\prolog_clause.pl
  File ${SWIPL}\library\prolog_xref.pl
  File ${SWIPL}\library\prolog_source.pl
  File ${SWIPL}\library\prolog_history.pl
  File ${SWIPL}\library\prolog_breakpoints.pl
  File ${SWIPL}\library\prolog_autoload.pl
  File ${SWIPL}\library\prolog_codewalk.pl
  File ${SWIPL}\library\prolog_metainference.pl
  File ${SWIPL}\library\prolog_colour.pl
  File ${SWIPL}\library\prolog_format.pl
  File ${SWIPL}\library\prolog_install.pl
  File ${SWIPL}\library\check_installation.pl
  File ${SWIPL}\library\predicate_options.pl
  File ${SWIPL}\library\git.pl
  File ${SWIPL}\library\prolog_pack.pl

; COMPATIBILITY
  File ${SWIPL}\library\backcomp.pl
  File ${SWIPL}\library\edinburgh.pl
  File ${SWIPL}\library\qpforeign.pl
  File ${SWIPL}\library\quintus.pl
  File ${SWIPL}\library\files.pl
  File ${SWIPL}\library\charsio.pl
  File ${SWIPL}\library\codesio.pl
  File ${SWIPL}\library\arithmetic.pl

; `STANDARD LIBRARIES'
  File ${SWIPL}\library\ctypes.pl
  File ${SWIPL}\library\gensym.pl
  File ${SWIPL}\library\lists.pl
  File ${SWIPL}\library\sort.pl
  File ${SWIPL}\library\ugraphs.pl
  File ${SWIPL}\library\occurs.pl
  File ${SWIPL}\library\ordsets.pl
  File ${SWIPL}\library\oset.pl
  File ${SWIPL}\library\assoc.pl
  File ${SWIPL}\library\rbtrees.pl
  File ${SWIPL}\library\nb_rbtrees.pl
  File ${SWIPL}\library\nb_set.pl
  File ${SWIPL}\library\operators.pl
  File ${SWIPL}\library\heaps.pl
  File ${SWIPL}\library\broadcast.pl
  File ${SWIPL}\library\error.pl
  File ${SWIPL}\library\pairs.pl
  File ${SWIPL}\library\record.pl
  File ${SWIPL}\library\settings.pl
  File ${SWIPL}\library\terms.pl
  File ${SWIPL}\library\apply_macros.pl
  File ${SWIPL}\library\apply.pl
  File ${SWIPL}\library\aggregate.pl
  File ${SWIPL}\library\pure_input.pl
  File ${SWIPL}\library\pio.pl
  File ${SWIPL}\library\coinduction.pl
  File ${SWIPL}\library\quasi_quotations.pl
  File ${SWIPL}\library\sandbox.pl
  File ${SWIPL}\library\modules.pl
  File ${SWIPL}\library\win_menu.pl
  File ${SWIPL}\library\console_input.pl

; WINDOWS
  File ${SWIPL}\library\dde.pl
  File ${SWIPL}\library\progman.pl
  File ${SWIPL}\library\registry.pl

; DEVELOPMENT
  File ${SWIPL}\library\edit.pl
  File ${SWIPL}\library\make.pl
  File ${SWIPL}\library\hotfix.pl
  File ${SWIPL}\library\explain.pl
  File ${SWIPL}\library\debug.pl
  File ${SWIPL}\library\portray_text.pl
  File ${SWIPL}\library\vm.pl
  File ${SWIPL}\library\check.pl
  File ${SWIPL}\library\checklast.pl
  File ${SWIPL}\library\checkselect.pl
  File ${SWIPL}\library\shell.pl

; WEB STUFF
  File ${SWIPL}\library\www_browser.pl
  File ${SWIPL}\library\url.pl
  File ${SWIPL}\library\utf8.pl
  File ${SWIPL}\library\base32.pl
  File ${SWIPL}\library\base64.pl

; MISC
  File ${SWIPL}\library\readln.pl
  File ${SWIPL}\library\readutil.pl
  File ${SWIPL}\library\streampool.pl
  File ${SWIPL}\library\option.pl
  File ${SWIPL}\library\date.pl
  File ${SWIPL}\library\main.pl
  File ${SWIPL}\library\csv.pl
  File ${SWIPL}\library\persistency.pl
  File ${SWIPL}\library\ansi_term.pl
  File ${SWIPL}\library\optparse.pl

; DCG
  SetOutPath $INSTDIR\library\dcg
  File ${SWIPL}\library\dcg\basics.pl

; UNICODE
  SetOutPath $INSTDIR\library\unicode
  File ${SWIPL}\library\unicode\blocks.pl
  File ${SWIPL}\library\unicode\unicode_data.pl

  SetOutPath $INSTDIR\doc
  File ${SWIPL}\doc\windows.html
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\index.html

  SetRegView ${BITS}
  WriteRegStr HKLM ${REGKEY} "home" "$INSTDIR"

  ; Write uninstaller
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog" "DisplayName" "SWI-Prolog (remove only)"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog" "UninstallString" '"$INSTDIR\uninstall.exe"'
  SetRegView lastused
  WriteUninstaller "uninstall.exe"
SectionEnd

Section "Documentation and Help-system"
  SectionIn 1 3
  SetOutPath $INSTDIR
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\MANUAL
  File ${SWIPL}\library\helpidx.pl
  File ${SWIPL}\library\help.pl
SectionEnd

Section "PDT support files"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\pdt_console.pl
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\pdt_console.dll
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\pdt.html
SectionEnd

Section "Unicode library (utf8proc)"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\unicode.pl
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\unicode4pl.dll
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\utf8proc.html
SectionEnd

Section "Archive library (libarchive)"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\archive.pl
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\archive4pl.dll
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\archive.html
SectionEnd

Section "Constraint Handling Rules"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\chr.pl
  SetOutPath $INSTDIR\library\chr
  File ${SWIPL}\library\chr\chr_runtime.pl
  File ${SWIPL}\library\chr\chr_messages.pl
  File ${SWIPL}\library\chr\chr_debug.pl
  File ${SWIPL}\library\chr\chr_op.pl
  File ${SWIPL}\library\chr\chr_translate.pl
  File ${SWIPL}\library\chr\pairlist.pl
  File ${SWIPL}\library\chr\a_star.pl
  File ${SWIPL}\library\chr\binomialheap.pl
  File ${SWIPL}\library\chr\builtins.pl
  File ${SWIPL}\library\chr\chr_hashtable_store.pl
  File ${SWIPL}\library\chr\clean_code.pl
  File ${SWIPL}\library\chr\find.pl
  File ${SWIPL}\library\chr\listmap.pl
  File ${SWIPL}\library\chr\guard_entailment.pl
  File ${SWIPL}\library\chr\chr_compiler_options.pl
  File ${SWIPL}\library\chr\chr_compiler_utility.pl
  File ${SWIPL}\library\chr\chr_compiler_errors.pl
  File ${SWIPL}\library\chr\chr_integertable_store.pl
  File ${SWIPL}\library\chr\README.TXT
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\chr
SectionEnd

Section "CLP"
  SectionIn 1 3
  SetOutPath $INSTDIR\library\clp
  File ${SWIPL}\library\clp\bounds.pl
  File ${SWIPL}\library\clp\clp_events.pl
  File ${SWIPL}\library\clp\clp_distinct.pl
  File ${SWIPL}\library\clp\simplex.pl
  File ${SWIPL}\library\clp\clpfd.pl
  File ${SWIPL}\library\clp\clpb.pl
SectionEnd

Section "CLP on real and rational numbers: CLP(Q,R)"
  SectionIn 1 3
  Delete $INSTDIR\library\clp\clpqr\ugraphs.pl
  SetOutPath $INSTDIR\library\clp
  File /r ${SWIPL}\library\clp\clpr
  File /r ${SWIPL}\library\clp\clpq
  File /r ${SWIPL}\library\clp\clpqr
  File ${SWIPL}\library\clp\clpr.pl
  File ${SWIPL}\library\clp\clpq.pl
SectionEnd

Section "Portability (YAP, SICStus, Ciao, BIM, IF/Prolog) support"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\fastrw.pl
  File ${SWIPL}\library\dialect.pl
  File /r ${SWIPL}\library\dialect
SectionEnd

Section "Demo files"
  SectionIn 1 3
  SetOutPath $INSTDIR
  File /r ${SWIPL}\demo
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\dlltest.dll
SectionEnd

Section "C/C++ Interface"
  SectionIn 1 3
  SetOutPath $INSTDIR\lib
!ifdef MINGW
  File ${SWIPL}\lib\libswipl.dll.a
  File ${SWIPL}\lib\libswipl.def
  File ${SWIPL}\lib\libswipl.lib
  File ${SWIPL}\lib\plterm.dll.a
  File /nonfatal ${SWIPL}\lib\libpthreadGC2.dll.a
!else
  File ${SWIPL}\lib\swipl.lib
  File ${SWIPL}\lib\plterm.lib
  File ${SWIPL}\lib\${PTHREAD}.lib
!endif
  SetOutPath $INSTDIR
  File /r ${SWIPL}\include
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\swipl-ld.exe
  File ${SWIPL}\bin\swipl-rc.exe
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\pl2cpp.html
SectionEnd

Section "JPL -- Java <-> Prolog"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\jpl.dll
  SetOutPath $INSTDIR\lib
  File ${SWIPL}\lib\jpl.jar
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\jpl.pl
  SetOutPath $INSTDIR\doc\packages
  File /r ${SWIPL}\doc\packages\jpl
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\jpl
SectionEnd

Section "XPCE graphics library"
  SectionIn 1 3
  SetOutPath $INSTDIR
  Delete $INSTDIR\xpce\prolog\lib\pce_common.pl
  File /r ${SWIPL}\xpce
  File ${SWIPL}\swipl-win.rc
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\pl2xpce.dll
  File ${SWIPL}\bin\xpce-stub.exe
SectionEnd

Section "Package CLIB"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\cgi.dll
  File ${SWIPL}\bin\crypt.dll
  File ${SWIPL}\bin\files.dll
  File ${SWIPL}\bin\sha4pl.dll
  File ${SWIPL}\bin\uri.dll
  File ${SWIPL}\bin\uuid.dll
  File ${SWIPL}\bin\memfile.dll
  File ${SWIPL}\bin\mime.dll
  File ${SWIPL}\bin\socket.dll
  File ${SWIPL}\bin\time.dll
  File ${SWIPL}\bin\readutil.dll
  File ${SWIPL}\bin\process.dll
  File ${SWIPL}\bin\streaminfo.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\cgi.pl
  File ${SWIPL}\library\crypt.pl
  File ${SWIPL}\library\filesex.pl
  File ${SWIPL}\library\sha.pl
  File ${SWIPL}\library\uri.pl
  File ${SWIPL}\library\uuid.pl
  File ${SWIPL}\library\memfile.pl
  File ${SWIPL}\library\mime.pl
  File ${SWIPL}\library\socket.pl
  File ${SWIPL}\library\prolog_server.pl
  File ${SWIPL}\library\random.pl
  File ${SWIPL}\library\time.pl
  File ${SWIPL}\library\process.pl
  File ${SWIPL}\library\udp_broadcast.pl
  File ${SWIPL}\library\streaminfo.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\clib.html
SectionEnd

Section "SSL Interface"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\ssl4pl.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\ssl.pl
# SetOutPath $INSTDIR\library\http
# File ${SWIPL}\library\http\http_ssl_plugin.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\ssl.html
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\ssl
SectionEnd

Section "ODBC Interface"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\odbc4pl.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\odbc.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\odbc.html
SectionEnd

Section "CQL database Interface"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File /r ${SWIPL}\library\cql
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\cql.html
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\cql
SectionEnd

Section "Google protocol buffers"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\protobufs.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\protobufs.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\protobufs.html
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\protobufs
SectionEnd

Section "SGML/XML/HTML parser"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\sgml2pl.dll
  SetOutPath $INSTDIR\library
  File /r ${SWIPL}\library\DTD
  File ${SWIPL}\library\sgml.pl
  File ${SWIPL}\library\sgml_write.pl
  File ${SWIPL}\library\xsdp_types.pl
  File ${SWIPL}\library\iso_639.pl
  File ${SWIPL}\library\xpath.pl
  File ${SWIPL}\library\pwp.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\sgml.html
SectionEnd

Section "RDF and Semantic Web Library"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\rdf_db.dll
  File ${SWIPL}\bin\turtle.dll
  File ${SWIPL}\bin\ntriples.dll

  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\rdf.pl
  File ${SWIPL}\library\rdf_parser.pl
  File ${SWIPL}\library\rdf_triple.pl
  File ${SWIPL}\library\rewrite.pl
  File ${SWIPL}\library\rdf_ntriples.pl
  File ${SWIPL}\library\rdf_write.pl
  File /r ${SWIPL}\library\semweb

  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\rdf2pl.html
  File ${SWIPL}\doc\packages\semweb.html
  File ${SWIPL}\doc\packages\modules.gif
SectionEnd

Section "HTTP Client/Server package"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\http_stream.dll
  File ${SWIPL}\bin\websocket.dll
  File ${SWIPL}\bin\json.dll
  SetOutPath $INSTDIR\library
  File /r ${SWIPL}\library\http
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\http.html
  File ${SWIPL}\doc\packages\httpserver.gif
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\http
SectionEnd

Section "Pengines"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\pengines.pl
  File ${SWIPL}\library\pengines_io.pl
  File ${SWIPL}\library\term_to_json.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\pengines.html
  File ${SWIPL}\doc\packages\penarch.png
  File ${SWIPL}\doc\packages\pltpsynch.png
  File ${SWIPL}\doc\packages\pltpruncolour.png
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\pengines
SectionEnd

Section "Table package"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\table.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\table.pl
  File ${SWIPL}\library\table_util.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\table.html
SectionEnd

Section "NLP package"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\porter_stem.dll
  File ${SWIPL}\bin\snowball.dll
  File ${SWIPL}\bin\double_metaphone.dll
  File ${SWIPL}\bin\isub.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\porter_stem.pl
  File ${SWIPL}\library\snowball.pl
  File ${SWIPL}\library\double_metaphone.pl
  File ${SWIPL}\library\isub.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\nlp.html
SectionEnd

Section "R-project interface"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\R.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\R.html
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\R
SectionEnd

Section "ZLIB package"
  SectionIn 1 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\zlib4pl.dll
  File ${SWIPL}\bin\${ZLIB}.dll
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\zlib.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\zlib.html
SectionEnd

Section "Unit test package"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\plunit.pl
  File ${SWIPL}\library\test_wizard.pl
  File ${SWIPL}\library\test_cover.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\plunit.html
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\plunit
SectionEnd

Section "Documentation package"
  SectionIn 1 3
  SetOutPath $INSTDIR\library
  File /r ${SWIPL}\library\pldoc
  SetOutPath $INSTDIR\library
  File ${SWIPL}\library\pldoc.pl
  File ${SWIPL}\library\doc_http.pl
  File ${SWIPL}\library\doc_latex.pl
  File ${SWIPL}\library\doc_files.pl
  SetOutPath $INSTDIR\doc\packages
  File ${SWIPL}\doc\packages\pldoc.html
  SetOutPath $INSTDIR\doc
  File /r ${SWIPL}\doc\Manual
  SetOutPath $INSTDIR\doc\packages\examples
  File /r ${SWIPL}\doc\packages\examples\pldoc
SectionEnd

!ifndef MINGW
Section "C Debugging Symbols (.pdb files)"
  SectionIn 3
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\cgi.pdb
  File ${SWIPL}\bin\swipl.pdb
  File ${SWIPL}\bin\memfile.pdb
  File ${SWIPL}\bin\streaminfo.pdb
  File ${SWIPL}\bin\mime.pdb
  File ${SWIPL}\bin\odbc4pl.pdb
  File ${SWIPL}\bin\plterm.pdb
  File ${SWIPL}\bin\swipl-win.pdb
  File ${SWIPL}\bin\sgml2pl.pdb
  File ${SWIPL}\bin\socket.pdb
  File ${SWIPL}\bin\time.pdb
  File ${SWIPL}\bin\readutil.pdb
  File ${SWIPL}\bin\process.pdb
  File ${SWIPL}\bin\ssl4pl.pdb
  File ${SWIPL}\bin\zlib4pl.pdb
  File ${SWIPL}\bin\archive4pl.pdb
  File ${SWIPL}\bin\sha4pl.pdb
  File ${SWIPL}\bin\uri.pdb
  File ${SWIPL}\bin\files.pdb
  File ${SWIPL}\bin\http_stream.pdb
  File ${SWIPL}\bin\json.pdb
  File ${SWIPL}\bin\snowball.pdb
  File ${SWIPL}\bin\isub.pdb
  File ${SWIPL}\bin\protobufs.pdb
  File ${SWIPL}\bin\plregtry.pdb
  File ${SWIPL}\bin\unicode4pl.pdb
SectionEnd
!endif

Section "Sources for system predicates"
  SectionIn 1 3
  SetOutPath $INSTDIR
  File /r ${SWIPL}\boot
  SetOutPath $INSTDIR\bin
  File ${SWIPL}\bin\mkboot.bat
SectionEnd

Section "Shell Extensions" SecShell
  SectionIn 1 2 3
  ClearErrors
  ; back up old value of .pl
  ReadRegStr $1 HKCR .${EXT} ""
  IfErrors Label1
  StrCmp $1 "" Label1
    StrCmp $1 "PrologFile" Label1
    WriteRegStr HKCR .${EXT} "backup_val" $1
  Label1:
  WriteRegStr HKCR .${EXT} "" "PrologFile"

  ReadRegStr $0 HKCR "PrologFile" ""
  IfErrors 0 readOK
    StrCpy $0 "";
  readOK:
  StrCmp $0 "" 0 skipNSIAssoc
	WriteRegStr HKCR "PrologFile" "" "Prolog Source"
	WriteRegStr HKCR "PrologFile\shell" "" "open"
	WriteRegStr HKCR "PrologFile\DefaultIcon" "" $INSTDIR\bin\swipl-win.exe,0
  skipNSIAssoc:
  ; OPEN
  WriteRegStr HKCR "PrologFile\shell\open\command" "" '"$INSTDIR\bin\swipl-win.exe" "%1"'
  ; Bind `edit' to call PceEmacs
  WriteRegStr HKCR "PrologFile\shell\pceEmacs" "" "Open in PceEmacs"
  WriteRegStr HKCR "PrologFile\shell\pceEmacs\command" "" '"$INSTDIR\bin\swipl-win.exe" -g start_emacs,send(@(pce),show_console,iconic),send(@(emacs),show_buffer_menu)'
  WriteRegStr HKCR "PrologFile\shell\pceEmacs\ddeexec" "" "edit %1"
  WriteRegStr HKCR "PrologFile\shell\pceEmacs\ddeexec\Application" "" "PceEmacs"
  WriteRegStr HKCR "PrologFile\shell\pceEmacs\ddeexec\Topic" "" "control"
!ifdef SHELL_DDE
  ; EDIT (these are not yet correct)
  ; CONSULT
  WriteRegStr HKCR "PrologFile\shell\consult" "" "Load Prolog Source"
  WriteRegStr HKCR "PrologFile\shell\consult\command" "" '"$INSTDIR\bin\swipl-win.exe'
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "" "consult('%1')"
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "Application" "prolog"
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "ifexec" ""
  WriteRegStr HKCR "PrologFile\shell\consult\ddeexec" "Topic" "control"
!endif

  SetRegView ${BITS}
  WriteRegStr HKLM ${REGKEY} fileExtension ${EXT}
  SetRegView lastused

  IfErrors 0 NoError
    MessageBox MB_OK "Could not write registry to register filetypes\n \
		      You may wish to retry the installation with\n \
		      sufficient privileges or accept the ${EXT} files\n \
		      cannot be opened from the shell and Prolog does not\n \
		      appear in the start menu"
NoError:
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
  SectionIn 1 2 3
  SetOutPath ${CWD}
  CreateDirectory "$SMPROGRAMS\${GRP}"
  IfFileExists "$SMPROGRAMS\${GRP}\XPCE.lnk" 0 NoOldXPCE
    Delete "$SMPROGRAMS\${GRP}\XPCE.lnk"
  NoOldXPCE:
  CreateShortCut "$SMPROGRAMS\${GRP}\Prolog.lnk" \
		 "$INSTDIR\bin\swipl-win.exe" \
		 "--win_app" \
		 "$INSTDIR\bin\swipl-win.exe" \
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


  SetRegView ${BITS}
  WriteRegStr HKLM ${REGKEY} group   ${GRP}
  WriteRegStr HKLM ${REGKEY} cwd     ${CWD}
  WriteRegStr HKLM ${REGKEY} context ${SHCTX}
  SetRegView lastused
SectionEnd

Section "Update library index"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\swipl-win.exe" -f none -g "make_library_index(swi(library)),halt"'
  ExecWait '"$INSTDIR\bin\swipl-win.exe" -f none -g "win_flush_filetypes,halt"'
SectionEnd

Section "Precompiled libraries"
  SectionIn RO			# do not allow to delete this
  ExecWait '"$INSTDIR\bin\swipl-win.exe" -f none -g qcompile_libraries,halt'
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
		      /SD IDYES IDNO Done

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
    MessageBox MB_OK "Deleting $SMPROGRAMS\${GRP}" /SD IDOK
    RMDir /r "$SMPROGRAMS\${GRP}"
  NoGrp:

  IfFileExists "$INSTDIR\bin\swipl-win.exe" 0 NoDir
    RMDir /r "$INSTDIR"
    goto Done

  NoDir:
    MessageBox MB_OK "Folder $INSTDIR doesn't seem to contain Prolog" /SD IDOK

  Done:
    DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\SWI-Prolog"
    DeleteRegKey HKLM ${REGKEY}
SectionEnd

################################################################
# FUNCTIONS
################################################################

Function .onInit

  ;Extract InstallOptions files
  ;$PLUGINSDIR will automatically be removed when the installer closes

  InitPluginsDir
  File /oname=$PLUGINSDIR\options.ini "options.ini"

  SetRegView ${BITS}
  ClearErrors
  Push $R0
  ReadRegStr $R0 HKLM ${REGKEY} "home"
  IfErrors +2 0
    StrCpy $INSTDIR $R0
  Pop $R0
  SetRegView lastused

  Push $R0
  ${GetParameters} $cmdLineParams
  ClearErrors
  ${GetOptions} $cmdLineParams '/?' $R0
  IfErrors +3 0
    MessageBox MB_OK "/S /GRP='SWI-Prolog' /EXT='pl' /INSTDIR='<Install Directory>'"
    Abort
  Pop $R0

FunctionEnd

################################################################
# Check 64-bit environment
# Note that NSIS is a 32-bit executable.  Such executables have
# set PROCESSOR_ARCHITEW6432 to IA64 or AMD64 on 64-bit platforms
################################################################

Function Check64
  ClearErrors
  ReadEnvStr ${ARCH} PROCESSOR_ARCHITEW6432
  IfErrors WrongArch
  StrCmpS ${ARCH} "AMD64" 0 WrongArch
    Return

WrongArch:
  MessageBox MB_OK \
	"Not an AMD64 version of Windows!$\r$\n\
	 This version of SWI-Prolog runs on 64-bits Windows$\r$\n\
	 using the AMD64/X64 architecture only"
  Quit
FunctionEnd

################################################################
# Handle customisation;  Settings are maintained in
#
#	HKLM ${REGKEY}
#
# Using the following mapping:
#
#	${EXT} fileExtension
################################################################

Function SetCustom
# Basic system info
  Call UserInfo

# Filename extension
  ReadRegStr ${EXT} HKLM ${REGKEY} fileExtension
  StrCmp ${EXT} "" 0 HasExt
    StrCpy ${EXT} "pl"
  HasExt:
  WriteINIStr $PLUGINSDIR\options.ini "Field 4" "State" ${EXT}

# Startmenu program group
  ReadRegStr ${GRP} HKLM ${REGKEY} group
  StrCmp ${GRP} "" 0 HasGroup
    StrCpy ${GRP} "SWI-Prolog"
  HasGroup:
  WriteINIStr $PLUGINSDIR\options.ini "Field 6" "State" ${GRP}

# Start the dialog
  Push ${TEMP1}
  InstallOptions::dialog "$PLUGINSDIR\options.ini"
  Pop ${TEMP1}
  Pop ${TEMP1}

# Get the results
  ReadINIStr ${EXT} $PLUGINSDIR\options.ini "Field 4" "State"
  ReadINIStr ${GRP} $PLUGINSDIR\options.ini "Field 6" "State"
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
    StrCpy ${SHCTX}  "current"
    Goto end

  done:
    StrCmp ${SHCTX} "all" 0 +2
      SetShellVarContext all

  end:
FunctionEnd

Function .onInstSuccess
  MessageBox MB_YESNO "Installation complete. View readme?" /SD IDNO IDNO NoReadme
  ExecShell "open" "$INSTDIR\doc\windows.html"
  NoReadme:
FunctionEnd

Function .onInstFailed
  MessageBox MB_OK "Installation failed.$\r$\n\
		    If you cannot resolve the issue or it is a bug in the$\r$\n\
		    installer, please contact bugs@swi-prolog.org"
FunctionEnd
