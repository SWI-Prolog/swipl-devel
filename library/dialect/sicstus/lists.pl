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
