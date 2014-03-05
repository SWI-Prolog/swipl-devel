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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_io, [test_io/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog core I/O

This module is a Unit test for   Prolog built-ins that process I/O. Most
of these these are in pre-unit test format in the main test.pl

@author	Jan Wielemaker
*/

test_io :-
	run_tests([ io
		  ]).

:- begin_tests(io).

test(eof_dom, [ sto(rational_trees),
		condition(access_file('/dev/null', exist)),
		error(domain_error(eof_action, abc))
	      ]) :-
	open('/dev/null', read, _In, [eof_action(abc)]).
test(eof2, [ sto(rational_trees),
	     condition(access_file('/dev/null', exist)),
	     error(permission_error(input, past_end_of_stream, _))
	   ]) :-
	setup_call_cleanup(
	    open('/dev/null', read, In, [eof_action(error)]),
	    (	get_code(In, _C1),
		get_code(In, _C2)
	    ),
	    close(In)).
test(set_after_close,
     [ sto(rational_trees),
       error(existence_error(stream, _))
     ]) :-
	open_null_stream(S),
	close(S),
	set_stream(S, timeout(0)).
test(double_close,
     [ sto(rational_trees),
       error(existence_error(stream, _))
     ]) :-
	open_null_stream(S),
	close(S),
	close(S).

:- end_tests(io).
