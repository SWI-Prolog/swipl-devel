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

xpce_message(Spec, Lines, Rest) :-
	pce_message(Spec, Lines, Rest).
xpce_message(context_error(Goal, Context, What)) -->
	[ '~w: ~w '-[Goal, What] ],
	pce_message_context(Context).
xpce_message(type_error(Goal, ArgN, Type, _Value)) -->
	[ '~w: argument ~w must be a ~w'-[Goal, ArgN, Type], nl ].
xpce_message(Spec, [Spec|Tail], Tail).

pce_error(Term) :-
	source_location(File, Line), !,
	message_to_string(Term, Str),
	(   user:exception(warning, warning(File, Line, Str), _)
	->  true
	;   format(user_error,
		   '[PCE/Prolog: (~w:~d)~n~t~8|~w]~n',
		   [File, Line, Str])
        ).
pce_error(Term) :-
	message_to_string(Term, Str),
        format(user_error, '[PCE/Prolog: ~w]~n', [Str]).

pce_warn(Term) :-
	pce_error(Term).

pce_info(Term) :-
	message_to_string(Term, Str),
	format(user_output, '~w~n', [Str]).

message_to_string(Term, Str) :-
	xpce_message(Term, Actions, []), !,
	actions_to_format(Actions, Fmt, Args),
	sformat(Str, Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Fmt-Args,nl], Fmt, Args) :- !.
actions_to_format([Fmt0-Args0,nl|Tail], Fmt, Args) :- !,
	actions_to_format(Tail, Fmt1, Args1),
	concat_atom([Fmt0, '~n', Fmt1], Fmt),
	append(Args0, Args1, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :- !,
	actions_to_format(Tail, Fmt1, Args1),
	atom_concat(Fmt0, Fmt1, Fmt),
	append(Args0, Args1, Args).
actions_to_format([Term|Tail], Fmt, Args) :-
	actions_to_format(Tail, Fmt1, Args1),
	atom_concat('~w', Fmt1, Fmt),
	append([Term], Args1, Args).


