/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Installation dependant parts of the prolog code
*/

%:- user:assert(library_directory('.')).
:- user:assert(library_directory(lib)).
:- user:assert(library_directory('~/lib/prolog')).
:- user:assert((
	library_directory(Lib) :-
		feature(home, Home),
		concat(Home, '/library', RawLib),
		absolute_file_name(RawLib, Lib))).


$default_editor(notepad) :-
	feature(arch, Arch),
	concat(_, win32, Arch), !.
$default_editor(vi).
