/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, VU University Amsterdam

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

:- module(test_solution_sequences,
	  [ test_solution_sequences/0
	  ]).


:- use_module(library(plunit)).
:- use_module(library(solution_sequences)).

test_solution_sequences :-
	run_tests([ ord_intersection,
		    is_ordset
		  ]).

:- begin_tests(order_by).

data(1, a, a1).
data(1, a, ax).
data(1, b, a2).
data(2, a, n1).
data(2, b, n2).
data(2, b, n0).

test(a, all(A-B-C ==
	    [1-a-a1, 1-a-ax, 1-b-a2, 2-a-n1, 2-b-n2, 2-b-n0])) :-
	order_by([asc(A)], data(A,B,C)).
test(d, all(A-B-C ==
            [2-a-n1, 2-b-n2, 2-b-n0, 1-a-a1, 1-a-ax, 1-b-a2])) :-
	order_by([desc(A)], data(A,B,C)).
test(ad, all(A-B-C ==
	    [1-b-a2, 1-a-a1, 1-a-ax, 2-b-n2, 2-b-n0, 2-a-n1])) :-
	order_by([asc(A),desc(B)], data(A,B,C)).

:- end_tests(order_by).
