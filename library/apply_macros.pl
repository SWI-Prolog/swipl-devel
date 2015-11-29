/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2014, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(apply_macros,
	  [ expand_phrase/2,		% :PhraseGoal, -Goal
	    expand_phrase/4		% :PhraseGoal, +Pos0, -Goal, -Pos
	  ]).
:- use_module(library(lists)).
:- use_module(library(occurs)).

/** <module> Goal expansion rules to avoid meta-calling

This module defines goal_expansion/2 rules to   deal with commonly used,
but fundamentally slow meta-predicates. Notable   maplist/2... defines a
useful set of predicates, but its  execution is considerable slower than
a traditional Prolog loop. Using this  library calls to maplist/2... are
translated into an call  to  a   generated  auxilary  predicate  that is
compiled using compile_aux_clauses/1. Currently this module supports:

	* maplist/2..
	* forall/2
	* once/1
	* ignore/1
	* phrase/2
	* phrase/3

The idea for this library originates from ECLiPSe and came to SWI-Prolog
through YAP.

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
	length(Lists, N),
	expand_closure_no_fail(Callable0, N, Callable1),
	(   Callable1 = _:_
	->  strip_module(Callable0, M, Callable),
	    NextGoal = M:NextCall
	;   Callable = Callable1,
	    NextGoal = NextCall
	),
	Callable =.. [Pred|Args],
	length(Args, Argc),
	length(Argv, Argc),
	length(Vars, N),
	MapArity is N + 1,
	format(atom(AuxName), '__aux_maplist/~d_~w+~d', [MapArity, Pred, Argc]),
	append(Lists, Args, AuxArgs),
	Goal =.. [AuxName|AuxArgs],

	AuxArity is N+Argc,
	prolog_load_context(module, Module),
	functor(NextCall, Pred, AuxArity),
	\+ predicate_property(Module:NextGoal, transparent),
	(   predicate_property(Module:Goal, defined)
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
	    compile_aux_clauses([BaseClause, NextClause])
	).

expand_closure_no_fail(Callable0, N, Callable1) :-
	'$expand_closure'(Callable0, N, Callable1), !.
expand_closure_no_fail(Callable, _, Callable).

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
	qcall_instantiated(Callable), !,
	expand_maplist(Callable, Lists, Goal).

%%	expand_apply(+GoalIn:callable, -GoalOut, +PosIn, -PosOut) is semidet.
%
%	Translation  of  simple  meta  calls    to   inline  code  while
%	maintaining position information. Note that once(Goal) cannot be
%	translated  to  `(Goal->true)`  because  this   will  break  the
%	compilation of `(once(X) ; Y)`.  A   correct  translation  is to
%	`(Goal->true;fail)`.       Abramo       Bagnara        suggested
%	`((Goal->true),true)`, which is both faster   and avoids warning
%	if style_check(+var_branches) is used.

expand_apply(forall(Cond, Action), Pos0, Goal, Pos) :-
	Goal = \+((Cond, \+(Action))),
	(   nonvar(Pos0),
	    Pos0 = term_position(_,_,_,_,[PosCond,PosAct])
	->  Pos = term_position(0,0,0,0, % \+
				[ term_position(0,0,0,0, % ,/2
						[ PosCond,
						  term_position(0,0,0,0, % \+
								[PosAct])
						])
				])
	;   true
	).
expand_apply(once(Once), Pos0, Goal, Pos) :-
	Goal = (Once->true),
	(   nonvar(Pos0),
	    Pos0 = term_position(_,_,_,_,[OncePos]),
	    compound(OncePos)
	->  Pos = term_position(0,0,0,0,	% ->/2
				[ OncePos,
				  F-T		% true
				]),
	    arg(2, OncePos, F),		% highlight true/false on ")"
	    T is F+1
	;   true
	).
expand_apply(ignore(Ignore), Pos0, Goal, Pos) :-
	Goal = (Ignore->true;true),
	(   nonvar(Pos0),
	    Pos0 = term_position(_,_,_,_,[IgnorePos]),
	    compound(IgnorePos)
	->  Pos = term_position(0,0,0,0,			% ;/2
				[ term_position(0,0,0,0,	% ->/2
						[ IgnorePos,
						  F-T		% true
						]),
				  F-T				% true
				]),
	    arg(2, IgnorePos, F),	% highlight true/false on ")"
	    T is F+1
	;   true
	).
expand_apply(Phrase, Pos0, Expanded, Pos) :-
	expand_phrase(Phrase, Pos0, Expanded, Pos), !.


%%	expand_phrase(+PhraseGoal, -Goal) is semidet.
%%	expand_phrase(+PhraseGoal, +Pos0, -Goal, -Pos) is semidet.
%
%	Provide goal-expansion for  PhraseGoal.   PhraseGoal  is  either
%	phrase(NonTerminals, List) or phrase(NonTerminals,  List, Tail).
%	This predicate is intended to inline calls to phrase and support
%	code analysis.
%
%	For example:
%
%	  ==
%	  ?- expand_phrase(phrase(("ab", rule)), List), Goal).
%	  Goal = (List=[97, 98|_G121], rule(_G121, [])).
%	  ==
%
%	@throws	Re-throws errors from dcg_translate_rule/2

expand_phrase(Phrase, Goal) :-
	expand_phrase(Phrase, _, Goal, _).

expand_phrase(phrase(NT,Xs), Pos0, NTXsNil, Pos) :- !,
	extend_pos(Pos0, Pos1),
	expand_phrase(phrase(NT,Xs,[]), Pos1, NTXsNil, Pos).
expand_phrase(Goal, Pos0, NewGoal, Pos) :-
	dcg_goal(Goal, NT,Xs0,Xs),
	nonvar(NT),
	body_pos(RulePos0, Pos0),
	catch(dcg_translate_rule((pseudo_nt --> NT), RulePos0, Rule, RulePos),
	      error(Pat,ImplDep),
	      ( \+ harmless_dcgexception(Pat),
		throw(error(Pat,ImplDep))
	      )),
	body_pos(RulePos, Pos1),
	Rule = (pseudo_nt(Xs0c,Xsc) :- NewGoal0),
	Goal \== NewGoal0,
	\+ contains_illegal_dcgnt(NT), !, % apply translation only if we are safe
	(   var(Xsc), Xsc \== Xs0c
	->  Xs = Xsc, NewGoal1 = NewGoal0, Pos2 = Pos1
	;   NewGoal1 = (NewGoal0, Xsc = Xs),
	    (	nonvar(Pos1)
	    ->	Pos2 = term_position(0,0,0,0,[Pos1,EF-ET]),
		arg(2, Pos1, EF),
		ET is EF+1
	    ;	true
	    )
	),
	(   var(Xs0c)
	->  Xs0 = Xs0c,
	    NewGoal = NewGoal1,
	    Pos = Pos2
	;   NewGoal = ( Xs0 = Xs0c, NewGoal1 ),
	    (	nonvar(Pos2)
	    ->	Pos = term_position(0,0,0,0,[SF-ST,Pos2]),
		arg(1, Pos2, ST),
		SF is ST-1
	    ;	true
	    )
	).

dcg_goal(phrase(NT,Xs0,Xs), NT, Xs0, Xs).
dcg_goal(call_dcg(NT,Xs0,Xs), NT, Xs0, Xs).

extend_pos(Var, Var) :-
	var(Var), !.
extend_pos(term_position(F,T,FF,FT,ArgPos0),
	   term_position(F,T,FF,FT,ArgPos)) :-
	append(ArgPos0, [T-T], ArgPos).

body_pos(RulePos, BodyPos) :-
	var(RulePos), var(BodyPos), !.
body_pos(RulePos, BodyPos) :-
	nonvar(RulePos), !,
	RulePos = term_position(_,_,_,_,[_,BodyPos]).
body_pos(term_position(0,0,0,0,[0-0,BodyPos]), BodyPos).


%%	qcall_instantiated(@Term) is semidet.
%
%	True if Term is instantiated sufficiently to call it.
%
%	@tbd	Shouldn't this be callable straight away?

qcall_instantiated(Var) :-
	var(Var), !,
	fail.
qcall_instantiated(M:C) :- !,
	atom(M),
	callable(C).
qcall_instantiated(C) :-
	callable(C).


harmless_dcgexception(instantiation_error).	% ex: phrase(([1],x:X,[3]),L)
harmless_dcgexception(type_error(callable,_)).	% ex: phrase(27,L)


%%	contains_illegal_dcgnt(+Term) is semidet.
%
%	True if Term contains a non-terminal   we cannot deal with using
%	goal-expansion. The test is too general approximation, but safe.

contains_illegal_dcgnt(NT) :-
	sub_term(I, NT),
	nonvar(I),
	illegal_dcgnt(I), !.

illegal_dcgnt(!).
illegal_dcgnt(phrase(_,_,_)).
illegal_dcgnt((_->_)).


		 /*******************************
		 *	      DEBUGGER		*
		 *******************************/

:- multifile
	prolog_clause:unify_goal/5.

prolog_clause:unify_goal(Maplist, Expanded, _Module, Pos0, Pos) :-
	is_maplist(Maplist),
	maplist_expansion(Expanded),
	Pos0 = term_position(F,T,FF,FT,[_MapPos|ArgsPos]),
	Pos  = term_position(F,T,FF,FT,ArgsPos).

is_maplist(Goal) :-
	compound(Goal),
	functor(Goal, maplist, A),
	A >= 2.

maplist_expansion(Expanded) :-
	compound(Expanded),
	functor(Expanded, Name, _),
	sub_atom(Name, 0, _, _, '__aux_maplist/').


		 /*******************************
		 *	     ACTIVATE		*
		 *******************************/

:- multifile
	system:goal_expansion/2,
	system:goal_expansion/4.

%	@tbd	Should we only apply if optimization is enabled (-O)?

system:goal_expansion(GoalIn, GoalOut) :-
	\+ current_prolog_flag(xref, true),
	expand_apply(GoalIn, GoalOut).
system:goal_expansion(GoalIn, PosIn, GoalOut, PosOut) :-
	expand_apply(GoalIn, PosIn, GoalOut, PosOut).

