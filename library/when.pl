/*  Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2004-2016, K.U.Leuven
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(when,
          [ when/2                      % +Condition, :Goal
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

%!  when(+Condition, :Goal)
%
%   Execute Goal when Condition is satisfied. I.e., Goal is executed
%   as by call/1  if  Condition  is   true  when  when/2  is called.
%   Otherwise  Goal  is  _delayed_  until  Condition  becomes  true.
%   Condition is one of the following:
%
%       * nonvar(X)
%       * ground(X)
%       * ?=(X,Y)
%       * (Cond1,Cond2)
%       * (Cond2;Cond2)
%
%   For example (note the order =a= and =b= are written):
%
%       ==
%       ?- when(nonvar(X), writeln(a)), writeln(b), X = x.
%       b
%       a
%       X = x
%       ==

when(Condition, Goal) :-
    '$eval_when_condition'(Condition, Optimised),
    trigger_first(Optimised, Goal).

%!  '$eval_when_condition'(+Condition, -Optimised)
%
%   C-building block defined in pl-attvar.c.   It  pre-processes the
%   when-condition, checks it  for   errors  (instantiation  errors,
%   domain-errors and cyclic terms) and   simplifies it. Notably, it
%   removes already satisfied conditions   from  Condition, unifying
%   Optimised to =true= if  there  is   no  need  to suspend. Nested
%   disjunctions are reported as or(List).


trigger_first(true, Goal) :-
    !,
    call(Goal).
trigger_first(nonvar(X), Goal) :-
    !,
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

%!  trigger_ground(@Term, :Goal)
%
%   Trigger Goal when Term becomes   ground.  The current implementation
%   uses nonground/2, waiting for  an   arbitrary  variable and re-check
%   Term  when  this  variable   is    bound.   Previous   version  used
%   term_variables and suspended  on  a   term  constructed  from  these
%   variables. It is clear  that  either   approach  performs  better on
%   certain types of terms. The term_variables/2  based approach wins on
%   large terms that are almost  ground.   Possibly  we need a nonground
%   that also returns the number of tests   performed  and switch to the
%   term_variables/2 based approach if this becomes large.

trigger_ground(X, Goal) :-
    (   nonground(X, V)
    ->  '$suspend'(V, when, trigger_ground(X, Goal))
    ;   call(Goal)
    ).

trigger_determined(X, Y, Goal) :-
    unifiable(X, Y, Unifier),
    !,
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


%!  check_disj(DisjVar, Disj, Goal)
%
%   If there is a disjunctive condition, we share a variable between the
%   disjunctions. If the goal is fired due to one of the conditions, the
%   shared variable is bound to (-).  Note   that  this implies that the
%   attributed variable is left  in   place.  The predicate when_goal//1
%   skips such goals on behalf of copy_term/3.

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

when_goals(det(trigger_determined(X, Y, G))) -->
    when_trigger_goal(?=(X,Y), G).
when_goals(call(Conj)) -->
    when_conj_goals(Conj).

when_conj_goals((A,B)) -->
    !,
    when_conj_goals(A),
    when_conj_goals(B).
when_conj_goals(when:G) -->
    when_goal(G).

when_goal(trigger_nonvar(X, G)) -->
    when_trigger_goal(nonvar(X), G).
when_goal(trigger_ground(X, G)) -->
    when_trigger_goal(ground(X), G).
when_goal(wake_det(_)) -->
    [].

when_trigger_goal(Cond, when:trigger(InnerCond, InnerGoal)) -->
    !,
    when_trigger_goal((Cond, InnerCond), InnerGoal).
when_trigger_goal(_, when:check_disj(Disj, _, _)) -->
    { Disj == (-) },
    !,
    [].
when_trigger_goal(_, when:check_disj(-, OrList, InnerGoal)) -->
    { OrList \== [] },
    !,
    { or_list(OrList, OrCond) },
    when_trigger_goal(OrCond, InnerGoal).
when_trigger_goal(Cond, Goal) -->
    [ when(Cond, Goal) ].

or_list([H], HCond) :-
    !,
    or_list_member(H, HCond).
or_list([H|T], (HCond;OT)) :-
    or_list_member(H, HCond),
    or_list(T, OT).

or_list_member(or(OrList), OrCond) :-
    !,
    or_list(OrList, OrCond).
or_list_member((L, R), (LCond, RCond)) :-
    !,
    or_list_member(L, LCond),
    or_list_member(R, RCond).
or_list_member(Cond, Cond).

:- multifile sandbox:safe_meta_predicate/1.

sandbox:safe_meta_predicate(when:when/2).
