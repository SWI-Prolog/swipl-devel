/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2010, University of Amsterdam,
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

:- module('$bags',
	  [ findall/3,			% +Templ, :Goal, -List
	    findall/4,			% +Templ, :Goal, -List, +Tail
	    bagof/3,			% +Templ, :Goal, -List
	    setof/3			% +Templ, :Goal, -List
	  ]).

:- meta_predicate
	findall(?, 0, -),
	findall(?, 0, -, ?),
	bagof(?, ^, -),
	setof(?, ^, -).

:- noprofile((
	findall/4,
	findall/3,
	bagof/3,
	setof/3,
	findall_loop/4)).

:- '$iso'((findall/3,
	   bagof/3,
	   setof/3)).

%%      findall(-Var, +Goal, -Bag) is det.
%%      findall(-Var, +Goal, -Bag, +Tail) is det.
%
%       Bag holds all alternatives for Var  in  Goal.   Bag  might  hold
%       duplicates.   Equivalent  to bagof, using the existence operator
%       (^) on all free variables of Goal.  Succeeds with Bag  =  []  if
%       Goal fails immediately.
%
%	The  findall/4  variation  is  a    difference-list  version  of
%	findall/3.

findall(Templ, Goal, List) :-
	findall(Templ, Goal, List, []).

findall(Templ, Goal, List, Tail) :-
	setup_call_cleanup(
	    '$new_findall_bag',
	    findall_loop(Templ, Goal, List, Tail),
	    '$destroy_findall_bag').

findall_loop(Templ, Goal, List, Tail) :-
	(   Goal,
	    '$add_findall_bag'(Templ)	% fails
	;   '$collect_findall_bag'(List, Tail)
	).

%%      bagof(+Var, +Goal, -Bag) is semidet.
%
%	Implements Clocksin and  Melish's  bagof/3   predicate.  Bag  is
%	unified with the alternatives of Var  in Goal, Free variables of
%	Goal are bound,  unless  asked  not   to  with  the  existential
%	quantifier operator (^).

bagof(Templ, Goal0, List) :-
	'$free_variable_set'(Templ^Goal0, Goal, Vars),
	(   Vars == v
	->  findall(Templ, Goal, List),
	    List \== []
	;   findall(Vars-Templ, Goal, Answers),
	    bind_bagof_keys(Answers,_),
	    keysort(Answers, Sorted),
	    pick(Sorted, Vars, List)
	).

bind_bagof_keys([], _).
bind_bagof_keys([W-_|WTs], Vars) :-
	term_variables(W, Vars, _),
	bind_bagof_keys(WTs, Vars).

pick(Bags, Vars1, Bag1) :-
	pick_first(Bags, Vars0, Bag0, RestBags),
	select_bag(RestBags, Vars0, Bag0, Vars1, Bag1).

select_bag([], Vars0, Bag0, Vars1, Bag1) :- !, % last one: deterministic
	Vars0 = Vars1,
	Bag0 = Bag1.
select_bag(_, Vars, Bag, Vars, Bag).
select_bag(RestBags, _, _, Vars1, Bag1) :-
	pick(RestBags, Vars1, Bag1).

%%	pick_first(+Bags, +Vars, -Bag1, -RestBags) is semidet.
%
%	Pick the first result-bag from the   list  of Templ-Answer. Note
%	that we pick all elements that are  equal under =@=, but because
%	the variables in the witness are canonized this is the same as ==.
%
%	@param Bags	List of Templ-Answer
%	@param Vars	Initial Templ (for rebinding variables)
%	@param Bag1	First bag of results
%	@param RestBags	Remaining Templ-Answer

pick_first([Vars-Templ|T0], Vars, [Templ|T], RestBag) :-
	pick_same(T0, Vars, T, RestBag).


pick_same([V-H|T0], Vars, [H|T], Bag) :-
	V == Vars, !,
	pick_same(T0, Vars, T, Bag).
pick_same(Bag, _, [], Bag).


%%      setof(+Var, +Goal, -Set) is semidet.
%
%	Equivalent to bagof/3, but sorts the   resulting bag and removes
%	duplicate answers. We sort  immediately   after  the  findall/3,
%	removing duplicate Templ-Answer pairs early.

setof(Templ, Goal0, List) :-
	'$free_variable_set'(Templ^Goal0, Goal, Vars),
	(   Vars == v
	->  findall(Templ, Goal, Answers),
	    Answers \== [],
	    sort(Answers, List)
	;   findall(Vars-Templ, Goal, Answers),
	    (	ground(Answers)
	    ->	sort(Answers,Sorted),
		pick(Sorted,Vars,List)
	    ;	bind_bagof_keys(Answers,_VDict),
		sort(Answers, Sorted),
		pick(Sorted, Vars, Listu),
		sort(Listu,List) % Listu ordering may be nixed by Vars
	    )
	).
