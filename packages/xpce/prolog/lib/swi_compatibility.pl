/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

:- module(pce_compatibility_layer,
	  [ strip_module/3,
	    auto_call/1,
	    callable_predicate/1,
	    modified_since_last_loaded/1,
	    pce_error/1,
	    pce_warn/1,
	    pce_info/1
	  ]).

:- module_transparent
	auto_call/1,
	strip_module/3,
	callable_predicate/1.

strip_module(T, M, G) :-
	'$strip_module'(T, M, G).

auto_call(G) :-
	G.


		 /*******************************
		 *      DIALOG EDITOR SUPPORT	*
		 *******************************/

%	callable_predicate(:Head)
%
%	Succeeds if Head can be called without raising an exception for
%	an undefined predicate

callable_predicate(Spec) :-
	strip_module(Spec, M, Head),
	default_module(M, Def),
	current_predicate(_, Def:Head), !.
callable_predicate(Spec) :-
	strip_module(Spec, _, Head),
	functor(Head, Name, Arity),
	'$in_library'(Name, Arity).

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


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- consult(library('english/pce_messages')).

:- multifile
	prolog:message/3.

prolog:message(Spec) -->
	pce_message(Spec).
prolog:message(context_error(Goal, Context, What)) -->
	[ '~w: ~w '-[Goal, What] ],
	pce_message_context(Context).
prolog:message(type_error(Goal, ArgN, Type, _Value)) -->
	[ '~w: argument ~w must be a ~w'-[Goal, ArgN, Type], nl ].

pce_error(Term) :-
	print_message(error, Term).

pce_warn(Term) :-
	print_message(warning, Term).

pce_info(Term) :-
	print_message(informational, Term).


