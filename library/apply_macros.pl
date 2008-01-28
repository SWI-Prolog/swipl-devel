/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(apply_macros,
	  [
	  ]).
:- use_module(library(lists)).
:- use_module(library(occurs)).

/** <module> Goal expansion rules to avoid meta-calling

This module defines goal_expansion/2 rules to   deal with commonly used,
but fundamentally slow meta-predicates. Notable   maplist/2... defines a
useful set of predicates, but its exection is considerable slower than a
traditional Prolog loop. Using this library   calls  to maplist/2... are
translated into an call  to  a   generated  auxilary  predicate  that is
compiled using compile_aux_clauses/1.  Currently this module supports:

	* maplist/2..
	* forall/2
	* once/1
	* ignore/1
	* phrase/2
	* phrase/3

@tbd	Support more predicates
@author	Jan Wielemaker
*/

:- dynamic
	user:goal_expansion/2.
:- multifile
	user:goal_expansion/2.


%%	expand_maplist(+Callable, +Lists, -Goal) is det.
%
%	Macro expansion for maplist/2 and higher arity.

expand_maplist(Callable0, Lists, Goal) :-
	(   Callable0 = _:_
	->  strip_module(Callable0, M, Callable),
	    NextGoal = M:NextCall
	;   Callable = Callable0,
	    NextGoal = NextCall
	),
	Callable =.. [Pred|Args],
	length(Args, Argc),
	length(Argv, Argc),
	length(Lists, N),
	length(Vars, N),
	MapArity is N + 1,
	format(atom(AuxName), '__aux_maplist/~d_~w+~d', [MapArity, Pred, Argc]),
	append(Lists, Args, AuxArgs),
	Goal =.. [AuxName|AuxArgs],

	AuxArity is N+Argc,
	prolog_load_context(module, Module),
	(   current_predicate(Module:AuxName/AuxArity)
	->  true
	;   empty_lists(N, BaseLists),
	    length(Anon, Argc),
	    append(BaseLists, Anon, BaseArgs),
	    BaseClause =.. [AuxName|BaseArgs],

	    heads_and_tails(N, NextArgs, Vars, Tails),
	    append(NextArgs, Argv, AllNextArgs),
	    NextHead =.. [AuxName|AllNextArgs],
	    append(Argv, Vars, PredArgs),
	    NextCall =.. [Pred|PredArgs],
	    append(Tails, Argv, IttArgs),
	    NextIterate =.. [AuxName|IttArgs],
	    NextClause = (NextHead :- NextGoal, NextIterate),
	    
	    (	predicate_property(NextGoal, transparent)
	    ->	compile_aux_clauses([ (:- module_transparent(Module:AuxName/AuxArity)),
				      BaseClause,
				      NextClause
				    ])
	    ;   compile_aux_clauses([BaseClause, NextClause])
	    )
	).


empty_lists(0, []) :- !.
empty_lists(N, [[]|T]) :-
	N2 is N - 1,
	empty_lists(N2, T).

heads_and_tails(0, [], [], []).
heads_and_tails(N, [[H|T]|L1], [H|L2], [T|L3]) :-
	N2 is N - 1,
	heads_and_tails(N2, L1, L2, L3).


%%	expand_apply(+GoalIn:callable, -GoalOut) is semidet.
%
%	Macro expansion for `apply' predicates.

expand_apply(Maplist, Goal) :-
	functor(Maplist, maplist, N),
	N >= 2,
	Maplist =.. [maplist, Callable|Lists],
	callable(Callable), !,
	expand_maplist(Callable, Lists, Goal).
expand_apply(forall(Cond, Action), \+((Cond, \+(Action)))).
expand_apply(once(Goal), (Goal->true)).
expand_apply(ignore(Goal), (Goal->true;true)).
expand_apply(phrase(NT,Xs), NTXsNil) :-
	expand_apply(phrase(NT,Xs,[]), NTXsNil).
expand_apply(phrase(NT,Xs0,Xs), NewGoal) :-
	Goal = phrase(NT,Xs0,Xs),
	nonvar(NT),
	catch('$translate_rule'((pseudo_nt --> NT), Rule),
	      error(Pat,ImplDep),
	      ( \+ harmless_dcgexception(Pat), 
		throw(error(Pat,ImplDep))
	      )),
	Rule = (pseudo_nt(Xs0c,Xsc) :- NewGoal0),
	Goal \== NewGoal0,
	\+ contains_illegal_dcgnt(NT), !,	% apply translation only if we are safe
	(   var(Xsc), Xsc \== Xs0c
	->  Xs = Xsc, NewGoal1 = NewGoal0
	;   NewGoal1 = (NewGoal0, Xsc = Xs)
	),
	(   var(Xs0c)
	-> Xs0 = Xs0c,
	   NewGoal = NewGoal1
	;  ( Xs0 = Xs0c, NewGoal1 ) = NewGoal
	).

harmless_dcgexception(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
harmless_dcgexception(type_error(callable,_)).	% ex: phrase(27,L)


%%	contains_illegal_dcgnt(+Term) is semidet.
%
%	True if Term contains a non-terminal   we cannot deal with using
%	goal-expansion. The test is too general approximation, but safe.

contains_illegal_dcgnt(NT) :-
	sub_term(I, NT),
	nonvar(I),
	( I = ! ; I = phrase(_,_,_) ), !.
%	write(contains_illegal_nt(NT)),		% JW: we do not want to write
%	nl.

		 /*******************************
		 *	     ACTIVATE		*
		 *******************************/

%	@tbd	Should we only apply if optimization is enabled (-O)?

user:goal_expansion(GoalIn, GoalOut) :-
	\+ current_prolog_flag(xref, true),
	expand_apply(GoalIn, GoalOut).
	
