/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

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

:- module($bags, [
	findall/3, 
	bagof/3, 
	setof/3]).

:- module_transparent
	findall/3, 
	setof/3, 
	bagof/3, 
	assert_bag/2.

%	findall(-Var, +Goal, -Bag)
%	Bag holds all alternatives for Var  in  Goal.   Bag  might  hold
%	duplicates.   Equivalent  to bagof, using the existence operator
%	(^) on all free variables of Goal.  Succeeds with Bag  =  []  if
%	Goal fails immediately.

findall(Var, Goal, Bag) :-
	assert_bag(v-Var, Goal),
	collect_bags([], [v-VarBag]), !,
	VarBag = Bag.
findall(_, _, []).

%	setof(+Var, +Goal, -Set
%	Equivalent to bagof/3, but sorts the resulting bag  and  removes
%	duplicate answers.

setof(Var, Goal, Set) :-
	bagof(Var, Goal, Bag), 
	sort(Bag, Set).

%	bagof(+Var, +Goal, -Bag)
%	Implements Clocksin and  Melish's  bagof/3  predicate.   Bag  is
%	unified  with the alternatives of Var in Goal, Free variables of
%	Goal are bound, unless asked not to with the existence  operator
%	(^).

bagof(Gen, Goal, Bag) :-
	$e_free_variables(Gen^Goal, Vars),
	assert_bag(Vars-Gen, Goal), 
	collect_bags([], Bags), 
	$member(Vars-Bag, Bags),
	Bag \== [].

assert_bag(Templ, G) :-
	$record_bag(-), 
	catch(G, E, $except_bag(E)),
	    $record_bag(Templ), 
	fail.
assert_bag(_, _).

collect_bags(Sofar, Result) :-
	$collect_bag(Vars, Bag), !,
	collect_bags([Vars-Bag|Sofar], Result).
collect_bags(L, L).
