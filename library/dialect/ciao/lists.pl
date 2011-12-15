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

:- module(ciao_lists,
	  [ nth/3,			% ?Index, ?List, ?Element
	    list_insert/2		% -List, +Term
	  ]).
:- reexport('../../lists').

%%	nth(?Index, ?List, ?Element) is nondet.
%
%	True if Element is the N-th element  in List. Counting starts at
%	1.
%
%	@deprecated use nth1/3.

nth(Index, List, Element) :-
	nth1(Index, List, Element).

%%	list_insert(-List, +Term)
%
%	Adds Term to the end of List  if   there  is  no element in List
%	identical to Term.

list_insert(List, Term) :-
        var(List), !,
        List=[Term|_].
list_insert([Term0|_], Term) :-
        Term0==Term, !.
list_insert([_|List], Term) :-
        list_insert(List, Term).
