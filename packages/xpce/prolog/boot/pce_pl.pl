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
	, auto_call/1
	, (meta_predicate)/1
	, (initialization)/1
	, 'pceloadc++'/1
	, 'pceloadc++'/2
	, 'pceloadcxx'/1
	, 'pceloadcxx'/2
	, '$call_atom'/1
	, pce_error/1
	, pce_warning/1
	, pce_info/1
	]).


:- module_transparent
	strip_module/3,
	'$load_pce'/0,
	require/1,
	auto_call/1.


:- use_module(library(quintus), [(meta_predicate)/1, (initialization)/1]).

		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

property(prolog(swi)).			% this is SWI-Prolog
property(file_extensions([pl])).	% list of file extensions
property(use_predicate_references).	% use direct predicate refs in methods
property(runtime) :-
	get(@(pce), is_runtime_system, @(on)).
	

		/********************************
		*         STRIP_MODULE		*
		********************************/

strip_module(Raw, Module, Term) :-
	'$strip_module'(Raw, Module, Term).


		 /*******************************
		 *	  PCELOAD/[1,2]		*
		 *******************************/

:- module_transparent
	pceloadcxx/1,
	pceloadcxx/2,
	'pceloadc++'/1,
	'pceloadc++'/2.

'pceloadc++'(File) :-
	'pceloadc++'(File, []).

'pceloadc++'(File, Libs) :-
	get(@(pce), home, Home),
	get(@(pce), machine, Machine),
	concat_atom([Home, '/pl/', Machine, '/pl-crt0.o'], Crt0),
	load_foreign(File, '__pl_start', '', [Crt0, '-lg++' | Libs], 0).

pceloadcxx(File) :-
	'pceloadc++'(File).

pceloadcxx(File, Libs) :-
	'pceloadc++'(File, Libs).


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

		 /*******************************
		 *	   AUTO_CALL/2		*
		 *******************************/

%	auto_call(+Goal).
%
%	In some cases, you donot want to load the (library) module required
%	to make some call at loadtime.  Examples in XPCE are common
%	references to emacs/[0,1], show_key_bindings/1, the help system,
%	etc.
%
%	SWI-Prolog defines autoloading and does not require this mechanism.
%	for many Prologs, the definition
%
%		autoloading(Goal) :-
%			functor(Goal, Name, Arity),
%			require([Name/Arity]),
%			Goal.

auto_call(Goal) :-
	Goal.


		/********************************
		*      DEBUGGER SUPPORT		*
		********************************/

%	$call_atom(+Atom)
%	Transform `Atom' into a term and call it in the user module.
%	Used to implement the `@' option in the PCE tracer.

'$call_atom'(Atom) :-
	term_to_atom(Term, Atom),
	user:Term.


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- consult('english/pce_messages').

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

pce_warning(Term) :-
	pce_error(Term).

pce_info(Term) :-
	message_to_string(Term, Str),
	format(user_output, '~w~n', [Str]).

message_to_string(Term, Str) :-
	xpce_message(Term, Actions, []),
	actions_to_format(Actions, Fmt, Args),
	sformat(Str, Fmt, Args).

actions_to_format([], '', []) :- !.
actions_to_format([nl], '', []) :- !.
actions_to_format([Fmt-Args,nl], Fmt, Args) :- !.
actions_to_format([Fmt0-Args0,nl|Tail], Fmt, Args) :- !,
	actions_to_format(Tail, Fmt1, Args1),
	concat_atom([Fmt0, '~n', Fmt1], Fmt),
	append(Args0, Args1, Args).
actions_to_format([Fmt0-Args0|Tail], Fmt, Args) :-
	actions_to_format(Tail, Fmt1, Args1),
	concat(Fmt0, Fmt1, Fmt),
	append(Args0, Args1, Args).


		/********************************
		*             ENTRY		*
		********************************/

'$load_pce' :-
	'$c_current_predicate'('$pce_init', user:'$pce_init'), !,
	set_feature(xpce, true),
	pce_principal:'$pce_init'.
'$load_pce' :-
	feature(dll, true), !,
	pce_principal:load_foreign_library(pl2xpce),
	set_feature(xpce, true).
'$load_pce' :-
	feature(open_shared_object, true), !,
	pce_principal:load_foreign_library(so(xpce4pl)),
	set_feature(xpce, true).


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
