/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: load dynamic linker
*/

:- module(dld,
	[ dld_link/1
	, dld_unlink/1
	, dld_call/1	  
	, dld_list_undefined/0
	, dld_function/2
	, dld_initialise/1
	]).

load_dld :-
	'$arch'(Mach, _),
	'$home'(SwiHome),
	concat_atom(['-L', SwiHome, '/library/', Mach], LibDir),
	load_foreign(library(dld),
	             dld_start,
		     LibDir,
		     '-ldld',
		     46000).

:- load_dld.

:- module_transparent dld_call/1.
