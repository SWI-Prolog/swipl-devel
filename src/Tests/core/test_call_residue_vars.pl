/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008-2015, University of Amsterdam
			      VU University Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_call_residue_vars,
	  [ test_call_residue_vars/0
	  ]).
:- use_module(library(plunit)).

test_call_residue_vars :-
	run_tests([ call_residue_vars
		  ]).

:- begin_tests(call_residue_vars).

test(freeze_in, Vars == [X]) :-
	call_residue_vars(freeze(X, true), Vars).
test(freeze_bind, Vars == []) :-
	call_residue_vars((freeze(X, true), X = 1), Vars).
test(freeze_out, Vars == []) :-
	x(X),
	freeze(X, true),
	call_residue_vars(true, Vars).
test(freeze_oi, Vars == [X]) :-
	x(X),
	freeze(X, true),
	call_residue_vars(freeze(X, fail), Vars).
test(nogc, Vars = [_]) :-
	call_residue_vars(gc_able, Vars).
test(gc, Vars = [_]) :-
	call_residue_vars((gc_able, garbage_collect), Vars).
test(gc2, Vars = [_]) :-
	call_residue_vars(gc_able2_gc, Vars).
test(modify, Vars == [X]) :-
	put_attr(X, a, 1),
	call_residue_vars(put_attr(X, a, 2), Vars).
test(trail, [all(Vars == [[]])]) :-
	G=(freeze(X,X=1),X=1),
	call_residue_vars(G,Vars),
	(true;Vars=[2]).
test(frozen_stacks, Vars == []) :-
	x(X),
	call_residue_vars(
	    (	freeze(X, true),
		nb_setval(x, a(b)),
		fail
	    ;   true
	    ),
	    Vars).
test(copy_term) :-
	T = x(X), put_attr(X, a, 1),
	copy_term(T, T2),
	garbage_collect,
	x(T2).
test(copy_term, Vars == [V]) :-
	T = x(X), put_attr(X, a, 1),
	call_residue_vars(copy_term(T, T2), Vars),
	arg(1, T2, V).
test(record) :-
	T = x(X), put_attr(X, a, 1),
	cp_record(T, T2),
	garbage_collect,
	x(T2).
test(record, Vars == [V]) :-
	T = x(X), put_attr(X, a, 1),
	call_residue_vars(cp_record(T, T2), Vars),
	arg(1, T2, V).

cp_record(T,T2) :-			% copy using recorded DB
	findall(T2, T2=T, [T2]).

x(_).					% avoid singleton warnings

gc_able :-
	gc_able2.

gc_able2 :-
	x(X),
	freeze(X, fail).

gc_able2_gc :-
        freeze(X, writeln(X)),
        garbage_collect.

:- end_tests(call_residue_vars).
