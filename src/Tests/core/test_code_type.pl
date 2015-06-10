/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2015, University of Amsterdam
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

:- module(test_code_type, [test_code_type/0]).
:- use_module(library(plunit)).
:- use_module(library(apply)).

/** <module> Test Prolog text code_typeting primitives

This module is a Unit test for  Prolog code_type/2, etc.

@author	Jan Wielemaker
*/

test_code_type :-
	run_tests([ code_type
		  ]).

:- begin_tests(code_type).

test(code_type, true) :-
	assert_ct,
	gen.

:- end_tests(code_type).

:- thread_local ct/2.

assert_ct :-
	retractall(ct(_,_)),
	forall(( between(0, 255, C),
		 code_type(C, T)
	       ),
	       assertz(ct(C,T))).

gen_t(T) :-
	ct(_C,T0),
	(   atom(T0)
	->  T = T0
	;   functor(T0,F,A),
	    functor(T,F,A)
	).

gen :-
	setof(T, gen_t(T), TL),
	maplist(gen, TL).

gen(T) :-
	(setof(C, code_type(C,T), CL) -> true ; CL = []),
	(setof(C, ct(C,T), CL2) -> true ; CL2 = []),
	(   CL == CL2
	->  true
	;   format('ERROR: code_type ~p: ~p \\== ~p~n', [T, CL, CL2]),
	    fail
	).
