/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Quintus compatibility predicates
*/

:- module(quintus, 
	[ unix/1
%	, file_exists/1

	, abs/2
	, sin/2
	, cos/2
	, tan/2
	, log/2
	, random/3

	, genarg/3

	, (mode)/1
	, (public)/1
	, (meta_predicate)/1
	, no_style_check/1
	, otherwise/0
	, numbervars/3
	, statistics/2
	]).

		/********************************
		*      SYSTEM INTERACTION       *
		*********************************/

%	unix(+Action)
%	interface to  Unix.

unix(system(Command)) :-
        shell(Command).
unix(shell(Command)) :-
        shell(Command).
unix(shell) :-
        shell.
unix(access(File, 0)) :-
        access_file(File, read).
unix(cd) :-
        chdir('~').
unix(cd(Dir)) :-
        chdir(Dir).
unix(args(L)) :-
	'$argv'(L).
unix(argv(L)) :-
	'$argv'(S),
	maplist(to_prolog, S, L).

to_prolog(S, A) :-
	name(S, L),
	name(A, L).


%	file_exists(+File)
%	Succeeds if `File' exists as a file or directory in the Unix file
%	system.

file_exists(File) :-
	exists_file(File).


		/********************************
		*        META PREDICATES        *
		*********************************/

%	otherwise/0
%	For (A -> B ; otherwise -> C)

otherwise.


		/********************************
		*          ARITHMETIC           *
		*********************************/

%	abs(+Number, -Absolute)
%	Unify `Absolute' with the absolute value of `Number'.

abs(Number, Absolute) :-
	Absolute is abs(Number).

%	Math library predicates

sin(A, V) :-	  V is sin(A).
cos(A, V) :-	  V is cos(A).
tan(A, V) :-	  V is tan(A).
log(A, V) :-	  V is log(A).
log10(X, V) :-	  V is log10(X).
pow(X,Y,V) :-	  V is X**Y.
ceiling(X, V) :-  V is ceil(X).
floor(X, V) :-	  V is floor(X).
round(X, V) :-	  V is round(X).
sqrt(X, V) :-	  V is sqrt(X).
acos(X, V) :-	  V is acos(X).
asin(X, V) :-	  V is asin(X).
atan(X, V) :-	  V is atan(X).
atan2(Y, X, V) :- V is atan(Y, X).
sign(X, V) :-	  V is sign(X).

%	random(+Min, +Max, -Value)

random(Min, Max, Value) :-
	Value is Min + random(Max).

		 /*******************************
		 *	TERM MANIPULATION	*
		 *******************************/


genarg(N, T, A) :-			% SWI-Prolog arg/3 is generic
	arg(N, T, A).

		 /*******************************
		 *	    STATISTICS		*
		 *******************************/

:- recorda('$runtime', 0, _).

statistics(runtime, [Total, New]) :- !,
	system:statistics(cputime, Time),
	Total is integer(Time * 1000),
	recorded('$runtime', Old, Ref),
	New is Total - Old,
	erase(Ref),
	recorda('$runtime', Total, _).
statistics(program, [InUse, _Free]) :- !,
	system:statistics(heapused, InUse).
statistics(heap, Stat) :- !,
	statistics(program, Stat).
statistics(global_stack, [InUse, Free]) :- !,
	system:statistics(globalused, InUse),
	system:statistics(globallimit, Limit),
	Free is Limit - InUse.
statistics(local_stack, [InUse, Free]) :- !,
	system:statistics(localused, InUse),
	system:statistics(locallimit, Limit),
	Free is Limit - InUse.
statistics(trail, [InUse]) :- !,
	system:statistics(trailused, InUse).


		/********************************
		*          STYLE CHECK          *
		*********************************/

q_style_option(single_var, singleton) :- !.
q_style_option(Option, Option).

no_style_check(QOption) :-
	q_style_option(QOption, SWIOption), 
	style_check(-SWIOption).

		/********************************
		*            OPERATORS          *
		*********************************/

:- op(0, fy, not).

		/********************************
		*         DIRECTIVES            *
		*********************************/

% :- op(1150, fx, [(mode), (public)]).

mode(_).
public(_).


		 /*******************************
		 *	TERM MANIPULATION	*
		 *******************************/

numbervars(Term, From, To) :-
	numbervars(Term, '$VAR', From, To).


		/********************************
		*            MODULES            *
		*********************************/

:- initialization op(1150, fx, (meta_predicate)).

:- module_transparent
	(meta_predicate)/1.

meta_predicate((Head, More)) :- !, 
	meta_predicate(Head), 
	meta_predicate(More).
meta_predicate(Spec) :-
	'$strip_module'(Spec, M, Head),
	meta_predicate(M, Head).

meta_predicate(M, Head) :-
	Head =.. [Name|Arguments], 
	member(Arg, Arguments), 
	module_expansion_argument(Arg), !, 
	functor(Head, Name, Arity), 
	module_transparent(M:Name/Arity).
meta_predicate1(_, _).		% just a mode declaration

module_expansion_argument(:).
module_expansion_argument(N) :- integer(N).
