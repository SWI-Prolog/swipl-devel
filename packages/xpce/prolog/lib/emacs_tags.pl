/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(emacs_tags,
	[ emacs_tag/3
	, emacs_tag_file/1
        , emacs_init_tags/1
	]).

:- use_module(library(pce)).
:- require([ concat/3
	   , forall/2
	   ]).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Make Emacs tags (produced with etags) information available to Prolog.
Predicates:

emacs_init_tags(+Directory)
  Reads the tags into the Prolog database.

emacs_tag(+Symbol, -File, -LineNo)
  Lookup tag in loaded tag-table
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- dynamic
	tag_string/1,
	tag_file/2,
	tag_dir/1.


:- pce_global(@emacs_tag_file_regex,
	      new(regex(string('\f\n\\([^,]+\\),\\sd+$')))).
:- pce_global(@emacs_tag_line_regex,
	      new(regex(string('.*%c\\(\\sd+\\)', 127)))).

emacs_tag(Name, File, LineNo) :-
	tag_string(String), !,
	new(Re, regex('')),
	get(Re, quote, Name, QName),
	(   send(Re, pattern, string('\\b%s\\b', QName))
	;   send(Re, pattern, string('\\b%s', QName))
	),
	get(Re, search, String, Start), !,
	send(@emacs_tag_file_regex, search, String, Start, 0),
	get(@emacs_tag_file_regex, register_value, String, 1, FNS),
	tag_dir(Dir),
	new(S, string('%s', Dir)),
	send(S, ensure_suffix, /),
	send(S, append, FNS),
	get(S, value, File),
	send(@emacs_tag_line_regex, match, String, Start),
	get(@emacs_tag_line_regex, register_value, String, 1, LNS),
	get(@pce, convert, LNS, int, LineNo).
	    

emacs_init_tags(Dir) :-
	concat(Dir, '/TAGS', TagFile),
	send(file(TagFile), exists),
	get(file(TagFile), time, TagDate),
	tag_file(TagFile, LoadedTagDate),
	send(LoadedTagDate, equal, TagDate), !.
emacs_init_tags(Dir) :-
	forall(tag_string(Str), free(Str)),
	retractall(tag_string(_)),
	retractall(tag_file(_, _)),
	retractall(tag_dir(_)),
	concat(Dir, '/TAGS', TagFile),
	get(file(TagFile), time, TagTime),
	send(TagTime, lock_object, @on),
	load_tags(TagFile),
	assert(tag_dir(Dir)),
	assert(tag_file(TagFile, TagTime)).
	

load_tags(File) :-
	new(F, file(File)),
	send(F, open, read),
	get(F, read, TagString),
	send(TagString, lock_object, @on),
	asserta(tag_string(TagString)),
	send(F, close),
	send(F, free).

emacs_tag_file(File) :-
	tag_file(File, _).
