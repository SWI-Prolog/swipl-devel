/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: file manipulation
*/

:- module(files,
	[ can_open_file/2
	]).

%	can_open_file(+Path, +Mode)
%	Succeeds if the user has access to `File' in mode `Mode'.  Fails
%	silently if this is not the  case.   `Mode'  is  one  of  {read,
%	write, both}.  This used to be difficult.  Since we have
%	access_file/2 it is merely a Quintus compatibility predicate
%	and should be in quintus.pl.  We will leave it here for compatibility
%	reasons.

can_open_file(File, read) :- !,
	access_file(File, read).
can_open_file(File, write) :- !,
	(   exists_file(File)
	->  access_file(File, write)
        ;   path_dir_name(File, Dir),
	    access_file(Dir, write)
	).
can_open_file(File, both) :-
	access_file(File, read),
	access_file(File, write).

path_dir_name(File, Dir) :-
	'$file_base_name'(File, Base),
	concat(RawDir, Base, File),
	(   RawDir == ''
	->  Dir = '.'
	;   Dir = RawDir
	).
