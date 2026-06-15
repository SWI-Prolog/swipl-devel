;; SWI-Prolog: NSIS support code for the .pl file association option.
;;
;; The "Open .pl files with SWI-Prolog" checkbox is Field 6 of the
;; custom NSIS.InstallOptions.ini.in we ship in cmake/.  After install,
;; SwiplMaybeAssoc reads its state from $PLUGINSDIR\NSIS.InstallOptions.ini
;; and, when checked, calls SwiplDoFileAssoc to register the
;; SWIProlog.SourceFile ProgID.
;;
;; Kept in a separate .nsh so the literal double-quote characters in
;; path strings and registry values do not pass through CMake string
;; parsing and CPack serialisation (which doesn't escape them).

Function SwiplMaybeAssoc
  ReadINIStr $0 "$PLUGINSDIR\NSIS.InstallOptions.ini" "Field 6" "State"
  StrCmp $0 "1" 0 swipl_skip_assoc
    Call SwiplDoFileAssoc
  swipl_skip_assoc:
FunctionEnd

Function SwiplDoFileAssoc
  WriteRegStr HKCR ".pl" "" "SWIProlog.SourceFile"
  WriteRegStr HKCR "SWIProlog.SourceFile" "" "SWI-Prolog Source File"
  WriteRegStr HKCR "SWIProlog.SourceFile\DefaultIcon" "" '"$INSTDIR\bin\swipl-win.exe",0'
  WriteRegStr HKCR "SWIProlog.SourceFile\shell\open\command" "" '"$INSTDIR\bin\swipl-win.exe" "%1"'
  WriteRegStr HKLM "Software\SWI\Prolog" "fileAssoc" "1"
  System::Call 'Shell32::SHChangeNotify(i 0x08000000, i 0, i 0, i 0)'
FunctionEnd
