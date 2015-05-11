/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2015, University of Amsterdam
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

:- module(test_io, [test_io/0]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test Prolog core I/O

This module is a Unit test for   Prolog built-ins that process I/O. Most
of these these are in pre-unit test format in the main test.pl

@author	Jan Wielemaker
*/

test_io :-
	run_tests([ io,
		    stream_pair
		  ]).

:- begin_tests(io, [sto(rational_trees)]).

test(eof_dom, [ condition(access_file('/dev/null', exist)),
		error(domain_error(eof_action, abc))
	      ]) :-
	open('/dev/null', read, _In, [eof_action(abc)]).
test(eof2, [ condition(access_file('/dev/null', exist)),
	     error(permission_error(input, past_end_of_stream, _))
	   ]) :-
	setup_call_cleanup(
	    open('/dev/null', read, In, [eof_action(error)]),
	    (	get_code(In, _C1),
		get_code(In, _C2)
	    ),
	    close(In)).
test(set_after_close,
     [ error(existence_error(stream, _))
     ]) :-
	open_null_stream(S),
	close(S),
	set_stream(S, timeout(0)).
test(double_close,
     [ error(existence_error(stream, _))
     ]) :-
	open_null_stream(S),
	close(S),
	close(S).
test(current_io_non_existing,
     [ error(existence_error(stream, unlikely))
     ]) :-
	current_input(unlikely).
test(current_io_closed,
     [ error(existence_error(stream, S))
     ]) :-
	open_null_stream(S),
	close(S),
	current_output(S).
test(input_is_not_output, fail) :-
	current_input(X),
	current_output(X).

:- end_tests(io).

:- begin_tests(stream_pair, [sto(rational_trees)]).

test(single, In == user_input) :-
	stream_pair(user_input, In, Out),
	assertion(var(Out)).
test(single, Out == user_output) :-
	stream_pair(user_output, In, Out),
	assertion(var(In)).
test(close, true) :-
	open_null_stream(S),
	stream_pair(Pair, user_input, S),
	close(Pair),
	assertion(catch(write(Pair, hello),
			error(existence_error(_,_),_),
			true)),
	stream_pair(Pair, _In, Out),
	assertion(var(Out)).

:- end_tests(stream_pair).
