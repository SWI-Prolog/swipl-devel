/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
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

:- module(test_module, [test_module/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog module handling

This module is a Unit test for Prolog module oddities.

@author	Jan Wielemaker
*/

test_module :-
	run_tests([ module
		  ]).

% test resetting the context module after a clause that uses
% I_CONTEXT

:- multifile
        test_module:cross/1.

:- test_module_2:assert((test_module:cross(_) :- nono)).
:- test_module_2:assert((nono :- fail)).

cross(M) :-
        context_module(M).

:- begin_tests(module).

test(cross, X == test_module) :-
	cross(X).

test(clause, H == test_moduld_2:term) :-
	assert(test_moduld_2:term, Ref),
	clause(H,_,Ref),
	erase(Ref).

test(clause, H == term) :-
	assert(test_moduld_2:term, Ref),
	test_moduld_2:clause(H,_,Ref),
	erase(Ref).

test(clause, H == term) :-
	assert(test_moduld_2:term, Ref),
	clause(test_moduld_2:H,_,Ref),
	erase(Ref).

:- end_tests(module).
