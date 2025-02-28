# SWI-Prolog in the browser

Public demo at https://wasm.swi-prolog.org/wasm/tinker

## TODO

 - Implement `?code=` and `?url=` (copy SWISH?)
 - Replace query input with CodeMirror instance.
 - Connect Prolog highlighting
 - Compile library to .qlf
 - Load additional (big) libraries from the server.  Examples
   - sCASP
   - CHR
   - ...
 - Turn tinker.js into one or more proper modules, using classes.
   - Allow for embedding, i.e., create a Prolog instance on a
     `<div>`, optionally holding the Prolog code.
 - Support more input predicates
   - get_code/1, etc.						[done]
   - library(readutil)
   - Wrap libraries automatically after they are loaded?
   - Replace toplevel emscripten conditional code with wrapping
 - Turn into a new GIT submodule?
 - Allow save/load from GitHub?
 - Add cls/0 and html/1 predicates to tinker.pl		[done]
 - Extend debugger
   - Support more commands
   - Sync source window with current location?
   - Nicer icons
 - Toplevel interaction
   - Add default toplevel actions
     - t, *, +, -, b, w, p
   - Show answer terms using HTML?
   - History search (Ctrl-R)?
   - Show history
   - Add button to reuse query					[done]
   - Input completion?  Currently minimal completion using
     [tab].  Should show box as you type.
   - How to deal with halt/0,1 and `end_of_file`?  Probably
     best to ignore.
 - Provide a dark mode

## Bugs

 - include/1 when loading from the web.				[fixed]
 - absolute_file_name/3 should handle URLs			[fixed]
