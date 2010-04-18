/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, VU University, Amsterdam

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

:- module(test_resource_error, [test_resource_error/0]).
:- use_module(library(plunit)).

/** <module> Test Resource overflows

This module is a Unit test for handling resource overflows.

@author	Jan Wielemaker
*/

test_resource_error :-
	run_tests([ resource_error
		  ]).

small_stacks(limits(LLimit, GLimit)) :-
	prolog_stack_property(local, limit(LLimit)),
	prolog_stack_property(global, limit(GLimit)),
	set_prolog_stack(local, limit(1*1024*1024)),
	set_prolog_stack(global, limit(1*1024*1024)).

restore_stacks(limits(LLimit, GLimit)) :-
	set_prolog_stack(local, limit(LLimit)),
	set_prolog_stack(global, limit(GLimit)).


:- begin_tests(resource_error,
	       [ setup(small_stacks(Restore)),
		 cleanup(restore_stacks(Restore))
	       ]).

choice.
choice.

local_overflow :-
	choice,
	local_overflow.

global_overflow(X) :-			% Causes gracefully signalled overflow
	global_overflow(s(X)).

string_overflow(StringList) :-
	string_overflow2(StringList),
	is_list(StringList).		% avoid GC of list

string_overflow2([H|T]) :-		% Causes PL_throw() overflow
	format(string(H), '~txx~1000000|', []),
	string_overflow2(T).


test(local, throws(error(resource_error(stack), local))) :-
	local_overflow.
					% VERY slow with -DO_SECURE
test(global, throws(error(resource_error(stack), global))) :-
	global_overflow(0).
test(string, throws(error(resource_error(stack), global))) :-
	catch(string_overflow(_), E1, true),
	E1 = error(resource_error(stack), global),
	string_overflow(_).
test(string, throws(error(resource_error(stack), global))) :-
	catch(string_overflow(_), E1, true),
	E1 = error(resource_error(stack), global),
	global_overflow(_).
test(length, throws(error(resource_error(stack), global))) :-
	length(L, 10000000),
	is_list(L).			% avoid GC

:- end_tests(resource_error).
