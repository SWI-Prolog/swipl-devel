/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U.Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the when/2 co-routine.
%
%%	when(+Condition, :Goal)
%
%		Condition should be one of
%			?=(X,Y)
%%			nonvar(X)
%%			ground(X)
%			(Condition,Condition)
%			(Condition;Condition)
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% History:
%
%	Apr 9, 2004
%	* JW: Supressed debugging this module
%	* JW: Made when/2 module-aware.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Simple implementation. Does not clean up redundant attributes.
% Now deals with cyclic terms.
%
% Currently, redundant constraints are skipped for copy_term/3. We could
% also   considering   to   implement   the   approach   of   block   in
% library(dialect/sicstus/block).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(when,
	  [ when/2			% +Condition, :Goal
	  ]).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	when(+, 0),
	suspend(-, 0).

:- use_module(library(error)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
when(Condition, Goal) :-
	when_condition(Condition, Optimised),
	trigger(Optimised, Goal).

when_condition(C, _) :-
	var(C), !,
	instantiation_error(C).
when_condition(?=(X,Y), C) :- !,
	(   ?=(X,Y)
	->  C = true
	;   C = ?=(X,Y)
	).
when_condition(nonvar(X), C) :- !,
	(   nonvar(X)
	->  C = true
	;   C = nonvar(X)
	).
when_condition(ground(X), C) :- !,
	(   ground(X)
	->  C = true
	;   C = ground(X)
	).
when_condition((C1,C2), C) :- !,
	when_condition(C1, T1),
	when_condition(C2, T2),
	conj(T1,T2,C).
when_condition((C1;C2), C) :- !,
	when_condition(C1, T1),
	(   T1 == true
	->  C = true
	;   when_condition(C2, T2),
	    (	T2 == true
	    ->	C = true
	    ;	C = (T1;T2)
	    )
	).
when_condition(C, _) :-
	domain_error(when_condition, C).

conj(true, C, C) :- !.
conj(C, true, C) :- !.
conj(C1, C2, (C1,C2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

trigger(true,Goal) :-
	call(Goal).
trigger(nonvar(X),Goal) :-
	trigger_nonvar(X,Goal).
trigger(ground(X),Goal) :-
	trigger_ground(X,Goal).
trigger(?=(X,Y),Goal) :-
	trigger_determined(X,Y,Goal).
trigger((G1,G2),Goal) :-
	trigger_conj(G1,G2,Goal).
trigger((G1;G2),Goal) :-
	trigger_disj(G1,G2,Goal).

trigger_nonvar(X,Goal) :-
	( nonvar(X) ->
		call(Goal)
	;
		suspend(X,trigger_nonvar(X,Goal))
	).

trigger_ground(X,Goal) :-
	term_variables(X,Vs),
	( Vs = [H] ->
		suspend(H,trigger_ground(H,Goal))
	; Vs = [H|_] ->
		T =.. [f|Vs],
		suspend(H,trigger_ground(T,Goal))
	;
		call(Goal)
	).

trigger_determined(X,Y,Goal) :-
	unifiable(X,Y,Unifier),
	!,
	( Unifier == [] ->
		call(Goal)
	;
		put_attr(Det,when,det(trigger_determined(X,Y,Goal))),
		suspend_list(Unifier,wake_det(Det))
	).

trigger_determined(_,_,Goal) :-
	call(Goal).


wake_det(Det) :-
	( var(Det) ->
		get_attr(Det,when,Attr),
		del_attr(Det,when),
		Det = (-),
		Attr = det(Goal),
		call(Goal)
	;
		true
	).

trigger_conj(G1,G2,Goal) :-
	trigger(G1,when:trigger(G2,Goal)).

trigger_disj(G1,G2,Goal) :-
	trigger(G1,when:check_disj(Disj,Goal)),
	trigger(G2,when:check_disj(Disj,Goal)).

%%	check_disj(DisjVar, Goal)
%
%	If there is a disjunctive condition, we share a variable between
%	the disjunctions. If the  goal  is  fired   due  to  one  of the
%	conditions, the shared variable is boud   to (-). Note that this
%	implies that the attributed  variable  is   left  in  place. The
%	predicate  when_goal//1  skips  such   goals    on   behalfe  of
%	copy_term/3.

check_disj(Disj,Goal) :-
	( var(Disj) ->
		Disj = (-),
		call(Goal)
	;
		true
	).

suspend_list([],_Goal).
suspend_list([V=W|Unifier],Goal) :-
	suspend(V,Goal),
	( var(W) -> suspend(W,Goal) ; true),
	suspend_list(Unifier,Goal).

suspend(V, Goal) :-
	(   get_attr(V, when, call(Goal0))
	->  put_attr(V, when, call((Goal,Goal0)))
	;   put_attr(V, when, call(Goal))
	).

attr_unify_hook(call(Goal), Other) :-
	(   get_attr(Other, when, call(GOTher))
	->  del_attr(Other, when),
	    Goal, GOTher
	;   Goal
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
attribute_goals(V) -->
	{ get_attr(V, when, Attr) },
	when_goals(Attr).

when_goals(det(trigger_determined(X, Y, G))) --> !,
	[when(?=(X,Y), G)].
when_goals(call(Conj)) -->
	when_conj_goals(Conj).

when_conj_goals((A,B)) --> !,
	when_conj_goals(A),
	when_conj_goals(B).
when_conj_goals(G) -->
	when_goal(G).

when_goal(when:trigger_ground(X, G)) --> unless_fired(G, when(ground(X), G)).
when_goal(when:trigger_nonvar(X, G)) --> unless_fired(G, when(nonvar(X), G)).
when_goal(when:wake_det(_))	     --> []. % ignore

unless_fired(G, Goal) -->
	(   { fired_disj(G) }
	->  []
	;   [Goal]
	).

fired_disj(when:check_disj(X, _)) :- X == (-).
fired_disj(when:check_disj(_, Goal)) :-
	fired_disj(Goal).
