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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(when,
	  [ when/2			% +Condition, :Goal
	  ]).
:- set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	when(+, 0),
	suspend_list(+, 0),
	trigger(+, 0),
	trigger_disj(+, 0),
	trigger_conj(+, +, 0).

/** <module> Conditional coroutining

This library implements the when/2 constraint, delaying a goal until its
arguments are sufficiently instantiated.  For   example,  the  following
delayes the execution of =:=/2 until the expression is instantiated.

    ==
	...
	when(ground(Expr), 0 =:= Expr),
    ==

@author Tom Schrijvers (initial implementation)
@author Jan Wielemaker
*/

%%	when(+Condition, :Goal)
%
%	Execute Goal when Condition is satisfied. I.e., Goal is executed
%	as by call/1  if  Condition  is   true  when  when/2  is called.
%	Otherwise  Goal  is  _delayed_  until  Condition  becomes  true.
%	Condition is one of the following:
%
%	    * nonvar(X)
%	    * ground(X)
%	    * ?=(X,Y)
%	    * (Cond1,Cond2)
%	    * (Cond2;Cond2)
%
%	For example (note the order =a= and =b= are written):
%
%	    ==
%	    ?- when(nonvar(X), writeln(a)), writeln(b), X = x.
%	    b
%	    a
%	    X = x
%	    ==

when(Condition, Goal) :-
	'$eval_when_condition'(Condition, Optimised),
	trigger_first(Optimised, Goal).

%%	'$eval_when_condition'(+Condition, -Optimised)
%
%	C-building block defined in pl-attvar.c.   It  pre-processes the
%	when-condition, checks it  for   errors  (instantiation  errors,
%	domain-errors and cyclic terms) and   simplifies it. Notably, it
%	removes already satisfied conditions   from  Condition, unifying
%	Optimised to =true= if  there  is   no  need  to suspend. Nested
%	disjunctions are reported as or(List).


trigger_first(true, Goal) :- !,
	call(Goal).
trigger_first(nonvar(X), Goal) :- !,
	'$suspend'(X, when, trigger_nonvar(X, Goal)).
trigger_first(Cond, Goal) :-
	trigger(Cond, Goal).

trigger(nonvar(X),Goal) :-
	trigger_nonvar(X,Goal).
trigger(ground(X),Goal) :-
	trigger_ground(X,Goal).
trigger(?=(X,Y),Goal) :-
	trigger_determined(X,Y,Goal).
trigger((G1,G2),Goal) :-
	trigger_conj(G1,G2,Goal).
trigger(or(GL),Goal) :-
	trigger_disj(GL, check_disj(_DisjID,GL,Goal)).

trigger_nonvar(X, Goal) :-
	(   nonvar(X)
	->  call(Goal)
	;   '$suspend'(X, when, trigger_nonvar(X, Goal))
	).

trigger_ground(X, Goal) :-
	term_variables(X, Vs),
	(   Vs = [H]
	->  '$suspend'(H, when, trigger_ground(H, Goal))
	;   Vs = [H|_]
	->  T =.. [f|Vs],
	    '$suspend'(H, when, trigger_ground(T, Goal))
	;   call(Goal)
	).

trigger_determined(X, Y, Goal) :-
	unifiable(X, Y, Unifier), !,
	(   Unifier == []
	->  call(Goal)
	;   put_attr(Det, when, det(trigger_determined(X,Y,Goal))),
	    suspend_list(Unifier, wake_det(Det))
	).
trigger_determined(_, _, Goal) :-
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
	trigger(G1, trigger(G2,Goal)).

trigger_disj([],_).
trigger_disj([H|T], G) :-
	trigger(H, G),
	trigger_disj(T, G).


%%	check_disj(DisjVar, Disj, Goal)
%
%	If there is a disjunctive condition, we share a variable between
%	the disjunctions. If the  goal  is  fired   due  to  one  of the
%	conditions, the shared variable is boud   to (-). Note that this
%	implies that the attributed  variable  is   left  in  place. The
%	predicate  when_goal//1  skips  such   goals    on   behalfe  of
%	copy_term/3.

check_disj(Disj,_,Goal) :-
	(   Disj == (-)
	->  true
	;   Disj = (-),
	    call(Goal)
	).

suspend_list([],_Goal).
suspend_list([V=W|Unifier],Goal) :-
	'$suspend'(V, when, Goal),
	(   var(W)
	->  '$suspend'(W, when, Goal)
	;   true
	),
	suspend_list(Unifier,Goal).

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
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = when:trigger(C, Goal) }
	->  [ when((?=(X,Y),C), Goal) ]
	;   [ when(?=(X,Y), G) ]
	).
when_goals(call(Conj)) -->
	when_conj_goals(Conj).

when_conj_goals((A,B)) --> !,
	when_conj_goals(A),
	when_conj_goals(B).
when_conj_goals(when:G) -->
	when_goal(G).

when_goal(trigger_nonvar(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = when:trigger(C, Goal) }
	->  [ when((nonvar(X),C), Goal) ]
	;   [ when(nonvar(X),G) ]
	).
when_goal(trigger_ground(X, G)) -->
	(   { disj_goal(G, Disj, DG) }
	->  disj_or(Disj, DG)
	;   { G = when:trigger(C, Goal) }
	->  [ when((ground(X),C), Goal) ]
	;   [ when(ground(X),G) ]
	).
when_goal(wake_det(_)) -->
	[].

disj_goal(when:check_disj(X, _, _), [], -) :- X == (-).
disj_goal(when:check_disj(-, Or, DG), Or, DG).

disj_or([], _) --> [].
disj_or(List, DG) -->
	{ or_list(List, Or) },
	[when(Or, DG)].

or_list([H], H) :- !.
or_list([H|T], (H;OT)) :-
	or_list(T, OT).
