/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
	->  write_ln(yes)
	;   write_ln(no)
	).

t3 :-
	A = foo(X),
	B = bar(X),
	write_ln((A=B)),
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
	write_ln(As).

t7 :-
	format('Please enter your name, followed by a dot~n', []),
	read(Name),
	format('Hello ~w~n', [Name]).
