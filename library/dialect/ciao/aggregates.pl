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

:- module(aggregates,
	  [ setof/3,
	    bagof/3,
	    findall/3,
	    findall/4,
	    findnsols/4,		% +N, ?Template, :Generator, -List
	    findnsols/5,		% +N, ?Template, :Generator, -List, -Tail
	    (^)/2
	  ]).

:- meta_predicate
	findnsols(+, ?, :, -),
	findnsols(+, ?, :, -, ?).

%%	findnsols(+N, ?Template, :Generator, -List)
%
%	As findall/3, but generating at most   N solutions of Generator.
%	Thus, the length of List will not   be  greater than N. If N=<0,
%	returns directly an empty  list.   This  predicate is especially
%	useful if Generator may have an infinite number of solutions.
%
%	@compat ciao

findnsols(N, Template, Generator, List) :-
	findnsols(N, Template, Generator, List, []).

%%	findnsols(+N, ?Template, :Generator, -List, -Tail)
%
%	As findnsols/4, but returning in Tail the tail of List.
%
%	@compat ciao

findnsols(N, Template, Generator, List, Tail) :-
	N > 0, !,
	findall(Template, maxsols(N, Generator), List, Tail).
findnsols(_, _, _, Tail, Tail).

maxsols(N, Generator) :-
	State = count(0),
	Generator,
	arg(1, State, C0),
	C1 is C0+1,
	(   C1 == N
	->  !
	;   nb_setarg(1, State, C1)
	).
