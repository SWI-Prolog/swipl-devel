/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2010-2011, University of Amsterdam
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
