:- module(prolog_cover,
	  [ covering_clauses/4		% +Goal, -Result, -Succeeded, -Failed
	  ]).
:- set_prolog_flag(generate_debug_info, false).

/** <module> Clause cover analysis

The purpose of this module is to find which part of the program has been
use by a certain goal. Usage is defined   in  terms of clauses that have
fired, seperated in clauses that  succeeded   at  least once and clauses
that failed on each occasion.

This module relies on the  SWI-Prolog   tracer  hooks. It modifies these
hooks and collects the results, after   which  it restores the debugging
environment.  This has some limitations:

	* The performance degrades significantly (about 10 times)
	* It is not possible to use the debugger using coverage analysis
	* The cover analysis tool is currently not thread-safe.

The result is  represented  as  a   list  of  clause-references.  As the
references to clauses of dynamic predicates  cannot be guaranteed, these
are omitted from the result.

@bug	Relies heavily on SWI-Prolog internals. We have considered using
	a meta-interpreter for this purpose, but it is nearly impossible
	to do 100% complete meta-interpretation of Prolog.  Example
	problem areas include handling cuts in control-structures
	and calls from non-interpreted meta-predicates.
*/


:- dynamic
	entered/1,			% clauses entered
	exited/1.			% clauses completed

:- module_transparent
	covering/4.

%%	covering_clauses(:Goal, -Result, -Succeeded, -Failed) is det
%
%	Run Goal as once/1. Unify Result with   one of =true=, =fail= or
%	error(Error). Unify Succeeded with  the   list  of  clauses that
%	succeeded at least once and Failed with the list of clauses that
%	always failed.

covering_clauses(Goal, Result, Succeeded, Failed) :-
	asserta(user:prolog_trace_interception(Port, Frame, _, continue) :-
			cover:assert_cover(Port, Frame), Ref),
	port_mask([unify,exit], Mask),
	'$visible'(V, Mask),
	'$leash'(L, Mask),
	trace,
	call_with_result(Goal, Result),
	set_prolog_flag(debug, false),
	covered(Ref, V, L, Succeeded, Failed).

%%	call_with_result(:Goal, -Result) is det.
%
%	Run Goal as once/1. Unify Result with   one of =true=, =fail= or
%	error(Error).

call_with_result(Goal, Result) :-
	(   catch(Goal, E, true)
	->  (   var(E)
	    ->	Result = true
	    ;	Result = error(E)
	    )
	;   Result = false
	).

port_mask([], 0).
port_mask([H|T], Mask) :-
	port_mask(T, M0),
	'$syspreds':'$port_bit'(H, Bit),	% Private stuff
	Mask is M0 \/ Bit.

%%	assert_cover(+Port, +Frame) is det.
%	
%	Assert coverage of the current clause. We monitor two ports: the
%	_unify_ port to see which  clauses   we  entered, and the _exit_
%	port to see which completed successfully.

assert_cover(unify, Frame) :-
	running_static_pred(Frame),
	prolog_frame_attribute(Frame, clause, Cl), !,
	assert_entered(Cl).
assert_cover(exit, Frame) :-
	running_static_pred(Frame),
	prolog_frame_attribute(Frame, clause, Cl), !,
	assert_exited(Cl).
assert_cover(_, _).

%%	running_static_pred(+Frame) is semidet.
%
%	True if Frame is not running a dynamic predicate.

running_static_pred(Frame) :-
	prolog_frame_attribute(Frame, goal, Goal),
	\+ predicate_property(Goal, dynamic).

%%	assert_entered(+Ref) is det.
%%	assert_exited(+Ref) is det.
%
%	Add Ref to the set of entered or exited	clauses.

assert_entered(Cl) :-
	entered(Cl), !.
assert_entered(Cl) :-
	assert(entered(Cl)).

assert_exited(Cl) :-
	exited(Cl), !.
assert_exited(Cl) :-
	assert(exited(Cl)).

%%	covered(+Ref, +VisibleMask, +LeashMask, -Succeeded, -Failed) is	det.
%
%	Restore state and collect failed and succeeded clauses.

covered(Ref, V, L, Succeeded, Failed) :-
	'$visible'(_, V),
	'$leash'(_, L),
	erase(Ref),
	findall(Cl, (entered(Cl), \+exited(Cl)), Failed),
	findall(Cl, retract(exited(Cl)), Succeeded),
	retractall(entered(Cl)).
