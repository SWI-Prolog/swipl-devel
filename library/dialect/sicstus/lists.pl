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

:- module(sicstus_lists,
	  [ substitute/4,		% +Elem, +List, +NewElem, -List
	    nth/3,			% ?N, ?List, ?Element
	    nth/4,			% ?N, ?List, ?Element, ?Rest
	    same_length/3,		% ?List1, ?List2, ?Length
	    sublist/2,			% ?Sub, +List
	    suffix/2,			% ?Suffix, ?List

	    % The following predicates are built-in on SWI.
	    % We re-export them here to avoid warnings
	    % when SICStus code explicitly imports them from library(lists).
	    is_list/1,			% +Term
	    memberchk/2			% +Element, +List
	  ]).
:- reexport('../../lists',
	    [ append/3,
	      delete/3,
	      last/2,
	      max_list/2,
	      member/2,
	      min_list/2,
	      nextto/3,
	      nth0/3,
	      nth0/4,
	      permutation/2,
	      prefix/2,
	      reverse/2,
	      same_length/2,
	      select/3,
	      sum_list/2
	    ]).

:- multifile sicstus:rename_module/2.

sicstus:rename_module(lists, sicstus_lists).

/** <module> SICStus 3-compatible library(lists).

@tbd	This library is incomplete.
	As of SICStus 3.12.11, the following predicates are missing:

	* no_doubles/1
	* non_member/2
	* remove_duplicates/2

@see	https://sicstus.sics.se/sicstus/docs/3.12.11/html/sicstus/Lists.html
*/

%%	substitute(+OldElem, +List, +NewElem, -NewList) is det.
%
%	NewList is List with all values that are identical (==) to OldElem
%	replaced by NewElem.

substitute(Old, List, New, NewList) :-
	substitute_(List, Old, New, NewList).

substitute_([], _, _, []).
substitute_([O|T0], Old, New, [V|T]) :-
	(   Old == O
	->  V = New
	;   V = O
	),
	substitute_(T0, Old, New, T).


%%	nth(?Index, ?List, ?Element) is nondet.
%
%	True if Element is the N-th element  in List. Counting starts at
%	1.
%
%	@deprecated use nth1/3.

nth(Index, List, Element) :-
	nth1(Index, List, Element).


%%	nth(?Index, ?List, ?Element, ?Rest) is nondet.
%
%	True if Element is the N-th element in List and Rest is the
%	remainder (as if by select/3) of List. Counting starts at 1.
%
%	@deprecated use nth1/4.

nth(Index, List, Element, Rest) :-
	nth1(Index, List, Element, Rest).


%%	same_length(?List1, ?List2, ?Length) is nondet.
%
%	True if List1 and List2 both have length Length.

same_length(List1, List2, Length) :-
	(   var(List1)
	->  length(List2, Length),
	    length(List1, Length)
	;   length(List1, Length),
	    length(List2, Length)
	).


%%	sublist(?Sub, +List)
%
%	True when all members of Sub  are   members  of List in the same
%	order.
%
%	@compat sicstus.  The order of generating sublists differs.
%	@compat This predicate is known in many Prolog implementations,
%		but the semantics differ. E.g. In YAP it is a continuous
%		sub-list.

sublist(Sub, List) :-
	sublist_(List, Sub).

sublist_([], []).
sublist_([H|T], Sub) :-
	sublist__(T, H, Sub).

sublist__([], H, [H]).
sublist__([], _, []).
sublist__([H|T], X, [X|Sub]) :-
	sublist__(T, H, Sub).
sublist__([H|T], _, Sub) :-
	sublist__(T, H, Sub).


%%	suffix(?Suffix, ?List) is nondet.
%
%	True if Suffix is a suffix of List. Not the same as suffix/2
%	in SICStus 4 - the arguments are reversed!

suffix(List, List).
suffix(Suffix, [_|Tail]) :-
	suffix(Suffix, Tail).
