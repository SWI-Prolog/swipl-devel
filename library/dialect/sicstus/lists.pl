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
	    nth/3,
	    sublist/2			% ?Sub, +List
	  ]).
:- reexport('../../lists').

:- multifile sicstus:rename_module/2.

sicstus:rename_module(lists, sicstus_lists).

%%	substitute(+OldElem, +List, +NewElem, -NewList) is det.
%
%	NewList is as List with all value that are identical (==) to OldElem
%	replaced by NewList.

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
