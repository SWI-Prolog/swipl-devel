/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
test file for the tracer package.  This file contains specific test
situations.  Test 1 2 3 4.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%:- ensure_loaded(trace).

:- dynamic status/1.

t1 :-
	trace,
	forall(between(0, 10, X), assert(status(X))),
	(   status(S),
	    S == 5
	).

t2 :-
	(   \+ foo = foo
	->  writeln(yes)
	;   writeln(no)
	).

t3 :-
	A = foo(X),
	B = bar(X),
	writeln((A=B)),
	true.

t4 :-
	a(X),
	b(Y), !,
	format('X=~w, Y=~w~n', [X, Y]).
t4.

a(1).
a(2).

b(1).
b(2).

t5 :-
	a(_A),
	forall(a(X), b(X)),
	b(_B).

t6 :-
	findall(A, a(A), As),
	writeln(As).

t7 :-
	format('Please enter your name, followed by a dot~n', []),
	read(Name),
	format('Hello ~w~n', [Name]).
