/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Program maintenance
*/

:- module($make,
	[ make/0
	]).

		/********************************
		*              MAKE             *
		*********************************/

%	make
%	Reload all source files that have been changed since they were
%	loaded.

make :-
	$update_library_index,
	$time_source_file(File, LoadTime),
	time_file(File, Modified),
	Modified @> LoadTime,
	reload(File),
	fail.
make.


%	reload(File)
%
%	Reload file into the proper module.  Note that if the file is loaded
%	into multiple modules this should be handled more carefully.

reload(File) :-
	findall(Context, $load_context_module(File, Context), Modules),
	(   Modules = []
	->  consult(user:File)
	;   Modules = [Module]
	->  consult(Module:File)
	;   Modules = [First|_Rest],
	    consult(First:File)
	).
	
