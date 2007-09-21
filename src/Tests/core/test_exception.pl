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

:- module(test_exception, [test_exception/0]).  
:- use_module(library(plunit)).

/** <module> Test Prolog core exception processing primitives

This module is a Unit test for   Prolog built-ins that handle exceptions
Please define a test-set for each predicate.

@author	Jan Wielemaker
*/

test_exception :-
	run_tests([ throw
		  ]).

:- begin_tests(throw).

test(error, error(instantiation_error)) :-
	throw(_).

test(ground, throws(hello(world))) :-
	throw(hello(world)).

test(unbound, [ setup(Ball = hello(_)),
		throws(Ball)
	      ]) :-
	throw(Ball).

test(cyclic, [ setup(Ball = hello(Ball)),
	       throws(Ball)
	     ]) :-
	throw(Ball).

:- end_tests(throw).
