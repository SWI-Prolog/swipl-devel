/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Installation dependant parts of the prolog code
*/

:- user:assert(library_directory('.')).
:- user:assert(library_directory(lib)).
:- user:assert(library_directory('~/lib/prolog')).
:- user:assert(library_directory('/usr/local/lib/pl/library')).
:- user:assert((
	library_directory(Lib) :-
		'$home'(Home),
		concat(Home, '/library', Lib))).


$default_editor(vi).
