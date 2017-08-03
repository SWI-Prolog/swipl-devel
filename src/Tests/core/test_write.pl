/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2016, University of Amsterdam
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

:- module(test_write, [test_write/0]).
:- use_module(library(plunit)).

/** <module> Misc tests

Tests that are hard to classify

@author	Jan Wielemaker
*/

test_write :-
	run_tests([ portray,
		    write_canonical,
		    write_variable_names,
		    write_float
		  ]).

:- begin_tests(portray).

user:portray(ptray_test) :-
	throw(nono).

test(exception, throws(nono)) :-
	print(ptray_test).
test(blob, Text == text) :-
	open_null_stream(S),
	with_output_to(atom(Text),
		       write_term(S, [ blobs(portray),
				       portray_goal(portray_stream)
				     ])),
	close(S).

portray_stream(S, _) :-
	stream_property(S, type(T)),
	format('~w', [T]).

:- end_tests(portray).

:- begin_tests(write_canonical).

test(numbervars, X = 'x(_,_)') :-
	with_output_to(atom(X),
		       write_canonical(x(_,_))).
test(numbervars, X = 'x(A,A)') :-
	with_output_to(atom(X),
		       write_canonical(x(B,B))).
test(numbervars, X = 'x(\'$VAR\'(1),_)') :-
	with_output_to(atom(X),
		       write_canonical(x('$VAR'(1),_))).
test(dot_in_atom, X == '\'a.b\'') :-
	with_output_to(atom(X),
		       write_canonical('a.b')).
test(dot_in_atom, L == [39,1080,46,1081,39]) :-
	atom_codes(A, [1080,46,1081]),
	with_output_to(atom(X),
		       write_canonical(A)),
	atom_codes(X,L).
test(varname, L == [39,1040,1080,39]) :-
	atom_codes(A, [1040,1080]),
	with_output_to(atom(X),
		       writeq(A)),
	atom_codes(X,L).

:- end_tests(write_canonical).

:- begin_tests(write_variable_names).

test(variable_names, X = 'a(B)') :-
	with_output_to(
	    atom(X),
	    write_term(a(A), [variable_names(['B'=A])])).
test(variable_names, error(type_error(atom, 1))) :-
	with_output_to(
	    atom(_),
	    write_term(a(A), [variable_names([1=A])])).
test(variable_names, error(domain_error(variable_name, '1'))) :-
	with_output_to(
	    atom(_),
	    write_term(a(A), [variable_names(['1'=A])])).
test(variable_names, X = 'a(\'$VAR\'(1),B)') :-
	with_output_to(
	    atom(X),
	    write_term(a('$VAR'(1), A),
		       [variable_names(['B'=A]), quoted(true)])).
test(variable_names, X = 'a(A,B)') :-
	with_output_to(
	    atom(X),
	    write_term(a('$VAR'(0), A),
		       [variable_names(['B'=A]), numbervars(true)])).

:- end_tests(write_variable_names).

:- begin_tests(write_float).

% the normal NaN should be written  as   1.5NaN.  On some systems (mips,
% hppa), it seems to write 1.49999...NaN. Possibly   due  to an issue in
% IEEE or os/dtoa.c.  We'll accept this output with a warning.

test(nan) :-
	A is nan,
	with_output_to(atom(X), write(A)),
	atom_concat(F, 'NaN', X),
	atom_number(F, Float),
	(   Float =:= 1.5
	->  true
	;   abs(abs(Float)-1.5) < 0.00001
	->  print_message(warning, write(nan, X))
	;   fail
	).

:- end_tests(write_float).

:- multifile
	prolog:message//1.

prolog:message(write(nan, X)) -->
	[ 'NaN is written as "~w"'-[X] ].
