/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, University of Amsterdam
                              VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
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
test(reuse_alias, error(permission_error(open, source_sink, alias(a)))) :-
	setup_call_cleanup(
	    ( open_null_stream(S),
	      set_stream(S, alias(a))
	    ),
	    open(nonexisting, read, _In, [alias(a)]),
	    close(S)).

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
