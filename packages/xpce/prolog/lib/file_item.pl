/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_file_item, []).
:- use_module(library(pce)).

:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).


		 /*******************************
		 *       FILE COMPLETION	*
		 *******************************/

:- pce_begin_class(file_item, text_item,
		   "text_item with file-name completion").

initialise(FI, Name:[name], Def:[any|function], Msg:[code]*) :->
	clean_file_name(Def, Clean),
	send(FI, send_super, initialise, Name, Clean, Msg),
	send(FI, style, combo_box).


activate(FI, Val:bool) :->
	(   Val == @on
	->  true
	;   send(FI, send_super, activate, Val)
	).


selected_completion(FI, Component:char_array, _Apply:[bool]) :->
	send(FI, send_super, selected_completion, Component, @off),
	get(FI, selection, Selection),
	(   send(directory(Selection), exists)
	->  send(FI, append, '/'),
	    send(FI, caret, @default)
	;   send(file(Selection), exists)
	->  send(FI, apply, @on)
	;   true
	).


completions(_FI, Tuple:tuple, Matches:chain) :<-
	"Chain with completions of FileName in DirName"::
	get(Tuple, first, DirName),
	get(Tuple, second, FileName),
	new(Matches, chain),
	new(Re, regex((string('^%s', FileName)))),
	(   send(class(file), has_feature, case_sensitive, @off)
	->  send(Re, ignore_case, @on)
	;   true
	),
	send(directory(DirName), scan, Matches, Matches, Re),
	send(Matches, delete_all, '.'),
	send(Matches, delete_all, '..').
	

split_completion(_FI, Value, Tuple:tuple) :<-
	"Split the current entry"::
	new(S, string('%s', Value)),
						% delete ...// or ...~
	get(S, size, L),
	(   get(regex('//\\|~\\|\\w:[/\\]'), search, S, L, 0, Start)
	->  send(S, delete, 0, Start),
	    (   send(S, prefix, '//')
	    ->  send(S, delete, 0, 1)
	    ;   true
 	    )
	;   true
	),
	
	(   send(S, suffix, /)
	->  Path = S,
	    BaseName = ''
	;   new(F, file(S)),
	    get(F, directory_name, DirName),
	    get(F, base_name, BaseName),
	    path(DirName, Path)
	),
	new(Tuple, tuple(Path, BaseName)).

path('', '') :- !.
path(Path, WithSlash) :-
	get(Path, ensure_suffix, /, WithSlash).


indicate_directory(_FI, Dir:string) :->
	(   send(directory(Dir), exists)
	->  send(Dir, ensure_suffix, /)
	;   true
	).


selection(FI, FileName:name) :<-
	"Get the current selection"::
	get(FI, get_super, selection, RawName),
	get(RawName, size, L),
	(   get(regex('//\\|~'), search, RawName, L, 0, Start)
	->  new(S, string('%s', RawName)),
	    send(S, delete, 0, Start),
	    (	send(S, prefix, '//')
	    ->  send(S, delete, 0, 1)
	    ;   true
 	    ),
	    get(S, value, FileName)
	;   get(RawName, value, FileName)
	).

clean_file_name(Def, Clean) :-
	\+ send(Def, '_instance_of', function),
	get(@pce, convert, Def, string, Clean), !,
	send(regex('//'), for_all, Clean,
	     message(@arg1, replace, @arg2, '/')).
clean_file_name(Def, Def).
	

browse(FI) :->
	"Run finder to fill with value"::
	get(FI?value_text?string, value, Sofar),
	(   sub_atom(Sofar, _, _, 0, /)
	->  Dir = Sofar
	;   file_directory_name(Sofar, Dir)
	),
	get(@finder, file, @on, directory := Dir, New),
	send(FI?value_text, string, New),
	send(FI, apply).

:- pce_end_class(file_item).


		 /*******************************
		 *     CLASS DIRECTORY_ITEM	*
		 *******************************/

:- pce_begin_class(directory_item, file_item).

completions(_FI, Tuple:tuple, Matches:chain) :<-
	"Chain with completions of FileName in DirName"::
	get(Tuple, first, DirName),
	get(Tuple, second, FileName),
	get(directory(DirName), directories, string('^%s', FileName), Matches).
	

:- pce_end_class.
