/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_host,
	[ '$load_pce'/0
	, strip_module/3
	, require/1
	, (meta_predicate)/1
	, initialization/1
	, 'pceloadc++'/1
	, 'pceloadc++'/2
	, '$call_atom'/1
	]).


:- module_transparent
	strip_module/3,
	'$load_pce'/0,
	require/1.


:- use_module(library(quintus), [(meta_predicate)/1, initialization/1]).

		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

property(prolog(swi)).			% this is SWI-Prolog
property(file_extensions([pl])).	% list of file extensions


		/********************************
		*         STRIP_MODULE		*
		********************************/

strip_module(Raw, Module, Term) :-
	'$strip_module'(Raw, Module, Term).


		 /*******************************
		 *	  PCELOAD/[1,2]		*
		 *******************************/

:- module_transparent
	'pceloadc++'/1,
	'pceloadc++'/2.

'pceloadc++'(File) :-
	'pceloadc++'(File, []).

'pceloadc++'(File, Libs) :-
	get(@(pce), home, Home),
	get(@(pce), machine, Machine),
	concat_atom([Home, '/pl/', Machine, '/pl-crt0.o'], Crt0),
	load_foreign(File, '__pl_start', '', [Crt0, '-lg++' | Libs], 0).


		/********************************
		*            REQUIRE		*
		********************************/

require([]) :- !.
require([H|T]) :- !,
	require(H),
	require(T).
require(_Name/_Arity) :- !.
require(Term) :-
	'$warning'('require/1: malformed argument: ~w', [Term]).


		/********************************
		*            BANNER		*
		********************************/

pce_reinitialise :-
	pce_boot:pce_reinitialise,
	format('~nFor HELP on prolog, please type help. or apropos(topic).~n'),
	format('         on xpce, please type manpce.~n~n').


		/********************************
		*      DEBUGGER SUPPORT		*
		********************************/

%	$call_atom(+Atom)
%	Transform `Atom' into a term and call it in the user module.
%	Used to implement the `@' option in the PCE tracer.

'$call_atom'(Atom) :-
	term_to_atom(Term, Atom),
	user:Term.


		/********************************
		*             ENTRY		*
		********************************/

'$load_pce' :-
	current_predicate('$pce_init', '$pce_init'), !,
	pce_principal:'$pce_init'.


		 /*******************************
		 *	  CONSULT-SUPPORT	*
		 *******************************/

%	callable_predicate(+Module:+Head)
%	True if Head is a callable predicate in Module.

callable_predicate(Module:Head) :-
	default_module(Module, DefModule),
	current_predicate(_, DefModule:Head), !.
callable_predicate(_:Head) :-
	functor(Head, Name, Arity),
	'$in_library'(Name, Arity).

default_module(M, M).
default_module(M, user) :- M \== user.
default_module(M, system) :- M \== system.

%	modified_since_last_loaded(Path)
%	True is file has been modified since the last time it was loaded.

modified_since_last_loaded(File) :-
	'$time_source_file'(File, LoadTime), !,
	time_file(File, Modified),
	Modified @> LoadTime.
modified_since_last_loaded(InFile) :-
	'$time_source_file'(File, LoadTime),
	same_file(InFile, File), !,
	time_file(File, Modified),
	Modified @> LoadTime.
