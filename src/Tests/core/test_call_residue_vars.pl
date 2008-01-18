/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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
test(freeze_out, Vars == []) :-
	x(X),
	freeze(X, true),
	call_residue_vars(true, Vars).
test(freeze_oi, [true(Vars == [X])]) :-
	x(X),
	freeze(X, true),
	call_residue_vars(freeze(X, fail), Vars).
test(nogc, [true(Vars = [_])]) :-
	call_residue_vars(gc_able, Vars).
test(gc, [true(Vars = [_])]) :-
	call_residue_vars((gc_able, garbage_collect), Vars).

x(_).					% avoid singleton warnings

gc_able :-
	gc_able2.

gc_able2 :-
	x(X),
	freeze(X, fail).

:- end_tests(call_residue_vars).
