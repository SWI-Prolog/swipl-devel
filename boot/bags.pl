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

:- module('$bags',
	  [ findall/3,			% +Templ, :Goal, -List
	    findall/4,			% +Templ, :Goal, -List, +Tail
	    bagof/3,			% +Templ, :Goal, -List
	    setof/3			% +Templ, :Goal, -List
	  ]).

:- module_transparent
	findall/4,
	findall/3,
	bagof/3,
	setof/3.

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
	strip_module(Goal, M, G),
	fa_local(Templ, M:G, List, Tail).

fa_local(Templ, M:G, List, Tail) :-
	setup_and_call_cleanup('$new_findall_bag'(Bag),
			       fa_loop(Templ, M:G, Bag, List, Tail),
			       '$destroy_findall_bag'(Bag)).
			       
fa_loop(Templ, Goal, Bag, List, Tail) :-
	\+ (Goal, \+ '$add_findall_bag'(Bag, Templ)),
	'$collect_findall_bag'(Bag, List, Tail).
	
%%      bagof(+Var, +Goal, -Bag) is semidet.
%
%       Implements Clocksin and  Melish's  bagof/3  predicate.   Bag  is
%       unified  with the alternatives of Var in Goal, Free variables of
%       Goal are bound, unless asked not to with the existence  operator
%       (^).

bagof(Templ, Goal, List) :-
	'$e_free_variables'(Templ^Goal, Vars),
	(   Vars == []
	->  findall(Templ, Goal, List),
	    List \== []
	;   findall(Vars-Templ, Goal, Answers),
	    keysort(Answers, Sorted),
	    pick(Sorted, Vars, List, _)
	).

pick(Bags, Vars1, Bag1, Resort1) :-
	pick_first(Bags, Vars0, Bag0, RestBags, Resort0),
	(   RestBags == []
	->  Bag1 = Bag0,
	    Vars1 = Vars0,
	    Resort1 = Resort0
	;   Bag1 = Bag0,
	    Vars1 = Vars0,
	    Resort1 = Resort0
	;   pick(RestBags, Vars1, Bag1, Resort1)
	).


%%	pick_first(+Bags, +Vars, -Bag1, -RestBags, -ReSort) is semidet.
%
%	Pick the first result-bag from the   list  of Templ-Answer. Note
%	that we pick all elements that are equal under =@=, but the keys
%	are sorted using sort/2 (standard order   of  terms). This means
%	that our results are not all  subsequent.   If  we can no longer
%	unify however, we are are too far and we can stop.
%
%	@param Bags	List of Templ-Answer
%	@param Vars	Initial Templ (for rebinding variables)
%	@param Bag1	First bag of results
%	@param RestBags	Remaining Templ-Answer
%	@param ReSort	If =true=, elements are picked out of order

pick_first([Vars-Templ|T0], Vars, [Templ|T], RestBag, ReSort) :-
	pick_same(T0, Vars, T, RestBag, ReSort).

pick_same([V-H|T0], Vars, [H|T], Bag, ReSort) :-
	V == Vars, !,
	pick_same(T0, Vars, T, Bag, ReSort).
pick_same([V-H|T0], Vars, [H|T], Bag, true) :-
	V =@= Vars, !,
	pick_same(T0, Vars, T, Bag, _).
pick_same([H|T0], Vars, Bag1, [H|Bag], true) :-
	arg(1, H, Key),
	\+ Vars \= Key, !,
	pick_same(T0, Vars, Bag1, Bag, _).
pick_same(Bag, _, [], Bag, false).


%%      setof(+Var, +Goal, -Set) is semidet.
%
%	Equivalent to bagof/3, but sorts the   resulting bag and removes
%	duplicate answers. We sort  immediately   after  the  findall/3,
%	removing duplicate Templ-Answer pairs early.

setof(Templ, Goal, List) :-
	'$e_free_variables'(Templ^Goal, Vars),
	(   Vars == []
	->  findall(Templ, Goal, Answers),
	    sort(Answers, List)
	;   findall(Vars-Templ, Goal, Answers),
	    sort(Answers, Sorted),
	    pick(Sorted, Vars, List0, ReSort),
	    re_sort(ReSort, List0, List)
	).

re_sort(true, List0, List) :- !,
	sort(List0, List).
re_sort(_, List, List).
