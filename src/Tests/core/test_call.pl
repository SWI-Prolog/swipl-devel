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

:- module(test_call, [test_call/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core meta-calling primitives

This module is  a  Unit  test  for   Prolog  built-ins  that  deal  with
meta-calling. Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_call :-
	run_tests([ call1,
		    apply,
		    callN,
		    cross_module_call,
		    snip,
		    no_autoload,
		    setup_call_cleanup
		  ]).

:- begin_tests(call1).

call1_a(X) :- X.			% These must be compiled the same.
call1_b(X) :- call(X).

test(call, X == 42) :-
	call1_a(X = 42).
test(clause, Body == call(X)) :-
	clause(call1_a(X), Body).
test(clause, Body == call(X)) :-
	clause(call1_b(X), Body).

:- end_tests(call1).

:- begin_tests(apply).

test(error, error(type_error(callable, 1))) :-
	apply(1, [a,b]).

:- end_tests(apply).


:- begin_tests(callN).

test(error, error(type_error(callable, 1))) :-
	call(1, a, b).

:- end_tests(callN).

cm1(X) :- context_module(X).
cm2(X) :- context_module(X).
:- export((cm1/1, cm2/1)).
:- user:(import((cm1/1, cm2/1))).	% import to user to call from foo:

:- begin_tests(cross_module_call).

cmc1:ok(cmc1).
cmc2:ok(cmc2).

cmc1(X) :-
	cmc1:ok(X).			% I_DEPARTM
cmc2(X) :-
	cmc2:ok(X),			% I_CALLM
	atom(X).
cmc3(X) :-
	cmc3:context_module(X).
cmc4(X) :-
	cmc4:context_module(X),
	atom(X).
c_cm1(X) :-
	foo:cm1(X).
c_cm2(X) :-
	foo:cm2(X).

test(cmc1, X == cmc1) :-
	cmc1(X).
test(cmc1, Body == cmc1:ok(X)) :-
	clause(cmc1(X), Body).
test(cmc2, X == cmc2) :-
	cmc2(X).
test(cmc2, Body == (cmc2:ok(X),atom(X))) :-
	clause(cmc2(X), Body).
test(cmc3, X == cmc3) :-
	cmc3(X).
test(cmc4, X == cmc4) :-
	cmc4(X).
test(c_cm1, X == test_call) :-
	c_cm1(X),
	1 = 1.				% avoid last-call
test(c_cm2, X == test_call) :-
	c_cm2(X).

:- end_tests(cross_module_call).


:- begin_tests(snip).

test(indent, [all(Gnosc == [scio])]) :-
	( true *-> Gnosc = scio, verum ; Gnosc = nescio ).

verum :-
	( false *-> true ; true ).

:- end_tests(snip).



:- begin_tests(no_autoload, [ setup(set_prolog_flag(autoload, false)),
			      cleanup(set_prolog_flag(autoload, true))
			    ]).

known(t) :-
	this_should_not_be_defined(V),
	call(V).

test(unknown, error(existence_error(procedure,
				    _:this_should_not_be_defined/1))) :-
	known(t).

:- end_tests(no_autoload).

:- begin_tests(setup_call_cleanup,
	       [ setup(retractall(v(_)))
	       ]).

:- dynamic
	v/1.

test(true, X == 42) :-
	setup_call_cleanup(A=42, true, assert(v(A))),
	retract(v(X)).

test(true_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	setup_call_cleanup(A=42, true, assert(v(A))),
	retract(v(X)).

test(cut, X == 42) :-
	setup_call_cleanup(A=42, (true;true), assert(v(A))), !,
	retract(v(X)).

test(cut_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	setup_call_cleanup(A=42, (true;true), assert(v(A))), !,
	retract(v(X)).

test(fail, X == 42) :-
	\+ setup_call_cleanup(A=42, fail, assert(v(A))),
	retract(v(X)).

test(fail2, X =@= [42,_]) :-
	\+ setup_call_cleanup(A=42, (B=2,fail), assert(v([A,B]))),
	retract(v(X)).

test(fail_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	\+ setup_call_cleanup(A=42, fail, assert(v(A))),
	retract(v(X)).

test(fail_debug2, [ true(X =@= [42,_]),
		    setup(debug),
		    cleanup(nodebug)
		  ]) :-
	\+ setup_call_cleanup(A=42, (B=2,fail), assert(v([A,B]))),
	retract(v(X)).

test(error, [X,E] == [42,error(x)]) :-
	catch(setup_call_cleanup(A=42, throw(error(x)), assert(v(A))),
	      E, true),
	retract(v(X)).

test(error_debug, [ true([X,E] == [42,error(x)]),
		    setup(debug),
		    cleanup(nodebug)
		 ]) :-
	catch(setup_call_cleanup(A=42, throw(error(x)), assert(v(A))),
	      E, true),
	retract(v(X)).

test(nondet, [Vs == [a,b,fail], cleanup(retractall(v(_)))]) :-
	(   setup_call_catcher_cleanup(true,
				       (ndet(X),
					assert(v(X))),
				       Exit,
				       assert(v(Exit))),
	    fail
	;   findall(V, retract(v(V)), Vs)
	).

ndet(a).
ndet(b).
ndet(_) :- 1 =:= 0.

test(cleanup, error(instantiation_error)) :-
	a(X),
	setup_call_cleanup(true, true, X).

test(cleanup, true) :-
	setup_call_cleanup(X=true, true, X).

test(error_choice, [throws(first)]) :-
	setup_call_cleanup(true, (G=1;G=2), throw(second)),
	throw(first).

test(error_choice, [throws(a(first))]) :-
	setup_call_cleanup(true, (G=1;G=2), throw(a(second))),
	throw(a(first)).

test(error_choice, [E+Xs =@= x+[x(1,_,_)]]) :-
	catch(test_error_choice, E, true),
	findall(X, retract(v(X)), Xs).

% this should undo the bindings of G and B before calling the
% cleanup handler.  I.e., S must be 1 and G and B must be var.

test_error_choice :-
	setup_call_cleanup(S=1,
			   (G=2;G=3),
			   assert(v(x(S,G,B)))),
	B = 4,
	throw(x).

a(_).

:- end_tests(setup_call_cleanup).
