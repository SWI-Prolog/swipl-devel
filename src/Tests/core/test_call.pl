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
	run_tests([ apply,
		    callN,
		    no_autoload,
		    setup_and_call_cleanup
		  ]).

:- begin_tests(apply).

test(error, error(type_error(callable, 1))) :-
	apply(1, [a,b]).

:- end_tests(apply).


:- begin_tests(callN).

test(error, error(type_error(callable, 1))) :-
	call(1, a, b).

:- end_tests(callN).


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

:- begin_tests(setup_and_call_cleanup,
	       [ setup(retractall(v(_)))
	       ]).

:- dynamic
	v/1.

test(true, X == 42) :-
	setup_and_call_cleanup(A=42, true, assert(v(A))),
	retract(v(X)).
     
test(true_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	setup_and_call_cleanup(A=42, true, assert(v(A))),
	retract(v(X)).
     
test(cut, X == 42) :-
	setup_and_call_cleanup(A=42, (true;true), assert(v(A))), !,
	retract(v(X)).
     
test(cut_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	setup_and_call_cleanup(A=42, (true;true), assert(v(A))), !,
	retract(v(X)).
     
test(fail, X == 42) :-
	\+ setup_and_call_cleanup(A=42, fail, assert(v(A))),
	retract(v(X)).
     
test(fail2, X =@= [42,_]) :-
	\+ setup_and_call_cleanup(A=42, (B=2,fail), assert(v([A,B]))),
	retract(v(X)).
     
test(fail_debug, [ true(X == 42),
		   setup(debug),
		   cleanup(nodebug)
		 ]) :-
	\+ setup_and_call_cleanup(A=42, fail, assert(v(A))),
	retract(v(X)).
     
test(fail_debug2, [ true(X =@= [42,_]),
		    setup(debug),
		    cleanup(nodebug)
		  ]) :-
	\+ setup_and_call_cleanup(A=42, (B=2,fail), assert(v([A,B]))),
	retract(v(X)).
     
test(error, [X,E] == [42,error(x)]) :-
	catch(setup_and_call_cleanup(A=42, throw(error(x)), assert(v(A))),
	      E, true),
	retract(v(X)).
     
test(error_debug, [ true([X,E] == [42,error(x)]),
		    setup(debug),
		    cleanup(nodebug)
		 ]) :-
	catch(setup_and_call_cleanup(A=42, throw(error(x)), assert(v(A))),
	      E, true),
	retract(v(X)).
     
:- end_tests(setup_and_call_cleanup).
