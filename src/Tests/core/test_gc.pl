/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(test_gc, [test_gc/0]).
:- use_module(library(plunit)).

/** <module> Test garbage collection

This unit contains small tests for the garbage collector.
*/

test_gc :-
	run_tests([ gc_leak
		  ]).

:- module_transparent
	space/2,
	nospace/1.

space(Goal, T+G) :-
	garbage_collect,
	statistics(trailused, T0),
	statistics(globalused, G0),
	Goal,
	garbage_collect,
	statistics(trailused, T1),
	statistics(globalused, G1),
	T is T1-T0,
	G is G1-G0.

nospace(G) :-
	space(G, Used),
	(   Used = (0+0)
	->  true
	;   format(user_error, '~p: Used ~w~n', [G, Used]),
	    fail
	).

:- begin_tests(gc_leak).

det_freeze :- 
	freeze(X, X==1), X=1.

test(attvar) :-
	nospace(det_freeze).

:- end_tests(gc_leak).

