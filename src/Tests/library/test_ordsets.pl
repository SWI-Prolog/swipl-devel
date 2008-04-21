/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_ordsets,
	  [ test_ordsets/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(ordsets)).

test_ordsets :-
	run_tests([ ord_intersection
		  ]).

:- begin_tests(ord_intersection).

test(oint4, [X,Y] == [[],[]]) :-
	ord_intersection([], [], X, Y).
test(oint4, [X,Y] == [[b],[]]) :-
	ord_intersection([a,b,c], [b], X, Y).
test(oint4, [X,Y] == [[a],[b,c]]) :-
	ord_intersection([a], [a,b,c], X, Y).
test(oint4, [X,Y] == [[b],[a,c]]) :-
	ord_intersection([b], [a,b,c], X, Y).
test(oint4, [X,Y] == [[c],[a,b]]) :-
	ord_intersection([c], [a,b,c], X, Y).


:- end_tests(ord_intersection).

