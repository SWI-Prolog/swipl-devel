/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam

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

:- module(arrays,
	  [ new_array/1,		% -Array
	    is_array/1,			% +Term
	    aref/3,			% +Index, +Array, -Element
	    arefa/3,			% +Index, +Array, -ArrayElement
	    arefl/3,			% +Index, +Array, -ListElement
	    array_to_list/2,		% +Array, -List
	    aset/4			% +Index, +Array, +Element, -NewArray
	  ]).
:- use_module(library(rbtrees)).

/** <module> SICStus 3 compatible array-library

@deprecated library(arrays) is dropped from SICStus
@compat	    This library builds on library(rbtrees) and therefore the
	    internal representation differs from the SICStus implementation.
*/


%%	new_array(-Array) is det.
%
%	@compat SICStus-3

new_array(array(Array)) :-
	rb_empty(Array).

%%	is_array(@Term) is semidet.
%
%	True if Term is an array
%
%	@compat SICStus-3

is_array(Var) :-
	var(Var), !, fail.
is_array(array(Tree)) :-
	is_rbtree(Tree).

%%	aref(+Index, +Array, -Element) is semidet.
%
%	True if Element is the current element in Array at Index.
%
%	@compat SICStus-3

aref(Index, array(Tree), Element) :-
	rb_lookup(Index, array(Tree), Element).

%%	arefa(+Index, +Array, -ArrayElement) is det.
%
%	As aref/3, but succeeds with an empty   array  of the element is
%	not set.
%
%	@compat SICStus-3

arefa(Index, array(Tree), ArrayElement) :-
	rb_lookup(Index, array(Tree), ArrayElement), !.
arefa(_, _, Array) :-
	new_array(Array).

%%	arefl(+Index, +Array, -ListElement) is det.
%
%	As aref/3, but succeeds with an empty list of the element is not
%	set.
%
%	@compat SICStus-3

arefl(Index, array(Tree), ListElement) :-
	rb_lookup(Index, array(Tree), ListElement), !.
arefl(_, _, []).

%%	array_to_list(+Array, -Pairs) is det.
%
%	@compat SICStus-3

array_to_list(array(Tree), Pairs) :-
	rb_visit(Tree, Pairs).

%%	aset(+Index, +Array, +Element, -NewArray) is det.
%
%	NewArray is Array with Element added/updated at Index.
%
%	@compat SICStus-3

aset(Index, array(Tree), Element, array(NewTree)) :-
	rb_update(Tree, Index, Element, NewTree), !.
aset(Index, array(Tree), Element, array(NewTree)) :-
	rb_insert_new(Tree, Index, Element, NewTree).
