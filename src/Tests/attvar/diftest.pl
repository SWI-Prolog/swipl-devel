/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

:- module(diftest,
	  [ diftest/0
	  ]).

dif(1) :-
	dif(1, A), \+ A = 1.
dif(2) :-
	dif(1, A), dif(2, A), \+ A = 1.
dif(3) :-
	dif(1, A), dif(2, A), \+ A = 2.
dif(4) :-
	dif(A, B), A = 1, \+ B = 1.
dif(5) :-
	A = a(A, 1),
	B = a(B, X),
	dif(A, B), \+ X = 1.
dif(6) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = a,
	\+ attvar(B).
dif(7) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,2),
	\+ B = 1.
dif(8) :-
	dif(a(x(1,2), B), a(X, 1)),
	X = x(1,Y),
	Y = 3,
	\+ attvar(B).
dif(9) :-
	dif(X, Y), \+ X = Y.
dif(10) :-
	dif(f(X,_Z),f(a,b)),
	dif(f(X,Y),f(b,b)),
	X = a, Y = b.


:- dynamic
	failed/1.

diftest :-
	retractall(failed(_)),
	forall(clause(dif(N), _, _),
	       (   dif(N)
	       ->  true
	       ;   format('~NFailed: ~w~n', [dif(N)]),
		   assert(failed(N))
	       )),
	\+ failed(_).
	
