/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    M-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_file_item, []).
:- use_module(library(pce)).

		 /*******************************
		 *       FILE COMPLETION	*
		 *******************************/

:- pce_begin_class(file_item, text_item,
		   "text_item with file-name completion").

completions(_FI, Tuple:tuple, Matches:chain) :<-
	"Chain with completions of FileName in DirName"::
	get(Tuple, first, DirName),
	get(Tuple, second, FileName),
	new(Matches, chain),
	send(directory(DirName), scan,
	     Matches, Matches, regex(string('^%s', FileName))).
	

split_completion(_FI, Value, Tuple:tuple) :<-
	"Split the current entry"::
	new(S, string('%s', Value)),
						% delete ...// or ...~
	get(S, size, L),
	(   get(regex('//\|~'), search, S, L, 0, Start)
	->  send(S, delete, 0, Start),
	    (   send(S, prefix, '//')
	    ->  send(S, delete, 0, 1)
	    ;   true
 	    )
	;   true
	),
	
	new(F, file(S)),
	get(F, directory_name, DirName),
	get(F, base_name, BaseName),
	path(DirName, Path),
	new(Tuple, tuple(Path, BaseName)).


path('', '') :- !.
path(Path, WithSlash) :-
	get(Path, ensure_suffix, /, WithSlash).


indicate_directory(_FI, Dir:string) :->
	(   send(directory(Dir), exists)
	->  send(Dir, ensure_suffix, /)
	;   true
	).


:- pce_end_class.

