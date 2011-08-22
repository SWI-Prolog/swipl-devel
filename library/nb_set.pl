/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2005, University of Amsterdam

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

:- module(nb_set,
	  [ empty_nb_set/1,		% -EmptySet
	    add_nb_set/2,		% +Key, !Set
	    add_nb_set/3,		% +Key, !Set, ?New
	    gen_nb_set/2,		% +Set, -Key
	    size_nb_set/2,		% +Set, -Size
	    nb_set_to_list/2		% +Set, -List
	  ]).


/** <module> Non-backtrackable sets

This library provides a non-backtrackabe set. It is based on
nb_setarg/3. See the SWI-Prolog manual for details.

@author Jan Wielemaker
@tbd	Base this work on AVL trees rather then unbalanced trees.
*/

		 /*******************************
		 *    NON-BACKTRACKABLE SETS	*
		 *******************************/

%%	empty_nb_set(-Set)
%
%	Create an empty non-backtrackable set.

empty_nb_set(nb_set(t)).

%%	add_nb_set(+Key, !Set) is det.
%%	add_nb_set(+Key, !Set, ?New) is semidet.
%
%	Insert an element into the set. If the element is already in the
%	set, nothing happens. New is =true= if   Key  was added as a new
%	element to the set and =false= otherwise.

add_nb_set(Key, Set) :-
	add_nb_set(Key, Set, _).
add_nb_set(Key, Set, New) :-
	(   empty_nb_set(Set)
	->  New = true,
	    nb_setarg(1, Set, t(Key, t, t))
	;   arg(1, Set, Tree),
	    '$btree_find_node'(Key, Tree, Node, Arg),
	    (	Arg == 1
	    ->	New = false
	    ;	New = true,
		nb_setarg(Arg, Node, t(Key, t, t))
	    )
	).


%%	nb_set_to_list(+Set, -List)
%
%	Get the elements of a an nb_set. List is sorted to the standard
%	order of terms.

nb_set_to_list(nb_set(Set), List) :-
	phrase(nb_set_to_list(Set), List).

nb_set_to_list(t) -->
	[].
nb_set_to_list(t(Val, Left, Right)) -->
	nb_set_to_list(Left),
	[Val],
	nb_set_to_list(Right).


%%	gen_nb_set(+Set, -Key)
%
%	Enumerate the members of a set in the standard order of terms.

gen_nb_set(nb_set(Tree), Key) :-
	gen_set(Tree, Key).

gen_set(t(Val, Left, Right), Key) :-
	(   gen_set(Left, Key)
	;   Key = Val
	;   gen_set(Right, Key)
	).

%%	size_nb_set(+Set, -Size)
%
%	Unify Size with the number of elements in the set

size_nb_set(nb_set(Tree), Size) :-
	set_size(Tree, Size).

set_size(t, 0).
set_size(t(_,L,R), Size) :-
	set_size(L, SL),
	set_size(R, SR),
	Size is SL+SR+1.
