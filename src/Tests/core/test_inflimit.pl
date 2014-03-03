/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2014, University of Amsterdam

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

:- module(test_inflimit,
	  [ test_inflimit/0
	  ]).
:- use_module(library(random)).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_inflimit :-
	run_tests([ call_with_inference_limit
		  ]).

outer :-
	call_with_inference_limit(inner(100), 500, Result),
	(   Result == inference_limit_exceeded
	->  p(+)
	;   format(user_error, '~NOops, no timeout (~w)!!~n', [Result]),
	    fail
	).

inner(N) :-
	forall(between(0, N, _),
	       forall(inner, true)).

inner :-
	Limit is random(100),
	call_with_inference_limit(goal, Limit, Result),
	(   Result == inference_limit_exceeded
	->  p(!)
	;   p(.)
	).

goal :-
	forall(between(1,25,_), true),
	maybe(0.3),				% may fail
	(   maybe(0.5)				% may succeed nondet
	;   true				% or det
	).

p(_) :- !.
p(C) :-	put_char(user_error, C).

:- begin_tests(call_with_inference_limit).

test(nesting, true) :-
	forall(between(1, 100, _), outer).

test(retry, true) :-
	call_with_inference_limit(length(_L,N), 1000, Result),
	forall(between(1,100,_), true),
	assertion(Result == true),
	N =:= 100, !.

:- end_tests(call_with_inference_limit).
