/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_autoload,
	[ pce_autoload/2
	, pce_autoload_all/0
	]).

:- use_module(pce_principal, [get/4, send/3]).

:- dynamic
	autoload/2.

%	pce_autoload(+ClassName, +FileSpec)
%
%	States class `ClassName' can be created by loading the Prolog
%	file `FileSpec'.  This will actually be done if either the class
%	is actually needed by PCE or pce_autoload_all/0 is called.

pce_autoload(Class, library(Library)) :- !,
	retractall(autoload(Class, _)),
	assert(autoload(Class, library(Library))).
pce_autoload(Class, Abs) :-
	concat('/', _, Abs), !,
	retractall(autoload(Class, _)),
	assert(autoload(Class, Abs)).
pce_autoload(Class, Abs) :-
	concat('$', _, Abs), !,
	retractall(autoload(Class, _)),
	assert(autoload(Class, Abs)).
pce_autoload(Class, Local) :-
	prolog_load_context(directory, Dir),
	pce_host:property(file_extensions(Exts)),
	member(E, Exts),
	ensure_extension(Local, E, Extended),
	concat_atom([Dir, /, Extended], File),
	exists_file(File), !,
	absolute_file_name(File, Abs),
	retractall(autoload(Class, _)),
	assert(autoload(Class, Abs)).
	
ensure_extension(File, Ext, Extended) :-
	concat('.', Ext, E),
	(   concat(_, E, File)
	->  Extended = File
	;   concat(File, E, Extended)
	).


%	pce_autoload_all/0
%
%	Load all    classes  declared  using   the    pce_autoload/2
%	directive.  Useful for debugging purposes.

pce_autoload_all :-
	autoload(Class, File),
	\+ get(@classes, member, Class, _),
	user:ensure_loaded(File),
	fail.
pce_autoload_all.


:- send(@pce?exception_handlers, append,
	attribute(undefined_class,
		  message(@prolog, call, trap_autoload, @arg1))).

trap_autoload(Class) :-
	autoload(Class, File),
	user:ensure_loaded(File).
