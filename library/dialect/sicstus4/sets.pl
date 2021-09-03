/*  Part of SWI-Prolog

    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(sets,
	  [ add_element/3,		% +Element, +Set1, -Set2
	    del_element/3,		% +Element, +Set1, -Set2
	    disjoint/2,			% +Set1, +Set2
	    intersect/2,		% +Set1, +Set2
	    set_order/3,		% +Xs, +Ys, -R
	    seteq/2,			% +Set1, +Set2
	    list_to_set/2,		% +List, -Set
	    disjoint_union/3,		% +Set1, +Set2, -Union
	    union/4			% +Set1, +Set2, -Union, -Difference
	  ]).
:- reexport(library(lists),
	    [ is_set/1,
	      subset/2,
	      intersection/3,
	      subtract/3,
	      union/3
	    ]).
:- use_module(library(lists), [selectchk/3]).

/** <module> SICStus 4 library(sets).

@tbd	This library is incomplete.
	As of SICStus 4.6.0, the following predicates are missing:

	* pairfrom/4
	* power_set/2
	* intersection/2
	* symdiff/3
	* setproduct/3
	* union/2

@see	https://sicstus.sics.se/sicstus/docs/4.6.0/html/sicstus.html/lib_002dsets.html
*/

add_element(Element, Set1, Set2) :-
	(   memberchk(Element, Set1)
	->  Set2 = Set1
	;   Set2 = [Element|Set1]
	).

del_element(Element, Set1, Set2) :-
	(   selectchk(Element, Set1, Set2)
	->  true
	;   Set2 = Set1
	).

disjoint(Set1, Set2) :-
	intersection(Set1, Set2, []).

intersect(Set1, Set2) :-
	intersection(Set1, Set2, Intersection),
	Intersection \= [].

set_order(Xs, Ys, R) :-
	(   subset(Xs, Ys)
	->  (   subset(Ys, Xs)
	    ->  R = (=)
	    ;   R = (<)
	    )
	;   (   subset(Ys, Xs)
	    ->  R = (>)
	    ;   fail
	    )
	).

seteq(Set1, Set2) :- set_order(Set1, Set2, =).

%%	list_to_set(+List, -Set) is det.
%
%	Set is List with all duplicates removed. Duplicates are removed
%	by unification. This is not the same as SWI-Prolog's
%	list_to_set/2 in library(lists), which finds duplicates based on
%	term equality (==).
%
%	@compat SICStus 4

list_to_set([], []).
list_to_set([X|Tail], Set) :-
	(   memberchk(X, Tail)
	->  Set = SetTail
	;   Set = [X|SetTail]
	),
	list_to_set(Tail, SetTail).

disjoint_union(Set1, Set2, Union) :-
	disjoint(Set1, Set2),
	append(Set1, Set2, Union).

union(Set1, Set2, Union, Difference) :-
	union(Set1, Set2, Union),
	subtract(Set1, Set2, Difference).
