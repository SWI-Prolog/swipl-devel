/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/

:- module(pce_global,
	[ pce_global/2				  % Ref x Goal
	]).

:- meta_predicate
      pce_global(+, :).


:- use_module(pce_principal, [send/3, object/1, new/2]).

:- require([strip_module/3, gensym/2, append/3]).

:- dynamic
	'pce global goal'/3,			  % Ref, Module, Goal
	'pce catcher'/2.			  % Module, Goal


		/********************************
		*            USER PART		*
		********************************/

%	pce_global(+@Reference, +Goal)
%	Register `Goal' to be a goal that creates `@Reference'

pce_global(@Ref, MGoal) :-
	strip_module(MGoal, Module, Goal),
	global(Ref, Module, Goal).

global(Ref, Module, Goal) :-
	var(Ref), !,
	retractall('pce catcher'(Module, Goal)),
	asserta('pce catcher'(Module, Goal)).
global(Ref, Module, Goal) :-			  % just reconsult
	'pce global goal'(Ref, Module, Goal), !.
global(Ref, Module, Goal) :-
	'pce global goal'(Ref, Module, _G2), !,	  % definition changed
	(   object(@Ref)
	->  gensym(Ref, NewRef),
	    send(@Ref, name_reference, NewRef),
	    format(user_error, ':- pce_global: Renamed @~w into @~w~n',
		   [Ref, NewRef])
	;   true
	),
	retractall('pce global goal'(Ref, Module, _G2)),
	asserta('pce global goal'(Ref, Module, Goal)).
global(Ref, _M1, new(Term)) :-			  % same definition
	'pce global goal'(Ref, _M2, new(Term)), !.
global(Ref, _M1, _G1) :-
	'pce global goal'(Ref, M2, _G2), !,
	format(user_error,
	       '[WARNING: Global object @~w already defined in module ~w]~n',
	       [Ref, M2]).
global(Ref, Module, Goal) :-
	asserta('pce global goal'(Ref, Module, Goal)).


		/********************************
		*            SYSTEM		*
		********************************/

:- send(@pce?exception_handlers, append,
	attribute(undefined_assoc, message(@prolog, call, trap_ref, @arg1))).

trap(@Ref) :-
	trap_ref(Ref).

trap_ref(Ref) :-
	'pce global goal'(Ref, Module, Goal), !,
	(   Goal = new(Term)
	->  (   new(@Ref, Module:Term)
	    ->  true
	    ;   format(user_error,
		       '[WARNING: pce_global/2: Create failed: ~w]~n',
		       [Term]),
		trace,
	        fail
	    )
	;   Goal =.. List,
	    append(List, [@Ref], GoalList),
	    GoalTerm =.. GoalList,
	    Module:GoalTerm
	->  true
	;   format(user_error,
		   '[WARNING: pce_global/2: Goal failed: ~w:~w]~n',
		   [Module, GoalTerm]),
	    trace,
	    fail
	).
trap_ref(Ref) :-
	'pce catcher'(Module, Goal),
	Goal =.. List,
	append(List, [@Ref], GoalList),
	GoalTerm =.. GoalList,
	Module:GoalTerm.
