/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
			      SWI-Prolog Solutions b.v.
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
:- use_module(library(debug)).

/** <module> Misc tests

Tests that are hard to classify

@author	Jan Wielemaker
*/

test_write :-
	run_tests([ portray,
		    write_op,
		    write_canonical,
		    write_quoted,
		    write_variable_names,
		    write_float,
		    write_misc
		  ]).

:- meta_predicate
	write_encoding(0, +, -).

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

:- begin_tests(write_op).

:- op(200, yf, {}).

test(nodict, X == 'f {x}') :-
	context_module(M),
	with_output_to(atom(X),
		       write_term(f {x}, [module(M)])).

:- end_tests(write_op).

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
test(braceterm, S=="{}(a)") :-
	with_output_to(string(S),
		       write_canonical({a})).
test(braceterm, S=="{}(','(a,b))") :-
	with_output_to(string(S),
		       write_canonical({a,b})).
test(quote, S=="'\u03B1'") :-		% quote_non_ascii
	with_output_to(string(S),
		       write_canonical('\u03B1')).
test(quote, S=="'\\x3B1\\'") :-
	write_encoding(write_canonical('\u03B1'),
		       ascii, S).

:- end_tests(write_canonical).

:- begin_tests(write_quoted).

test(comment, S == "'/*'") :-
	with_output_to(string(S), writeq('/*')).
test(comment, S == "'/*+'") :-
	with_output_to(string(S), writeq('/*+')).
test(comment, S == "+/*") :-		% Quote /* only when at start
	with_output_to(string(S), writeq('+/*')).
test(comment, S == "'%'") :-
	with_output_to(string(S), writeq('%')).
test(escape, S == "\u03B1") :-		  % Greek Aplha character
	with_output_to(string(S), write_term('\u03B1', [quoted(true)])).
test(escape, S == "'\u03B1'") :-
	with_output_to(string(S),
		       write_term('\u03B1',
				  [ quoted(true),
				    quote_non_ascii(true)
				  ])).
test(escape, S == "'\\u03B1'") :-
	assertion(current_prolog_flag(character_escapes_unicode, true)),
	write_encoding(write_term('\u03B1', [quoted(true)]), ascii, S).
test(escape, S == "'\\x3B1\\'") :-
	write_encoding(write_term('\u03B1',
				  [ quoted(true),
				    character_escapes_unicode(false)
				  ]),
		       ascii, S).
test(space, S == "= (is)") :-
	with_output_to(string(S),
		       (   write('= '),
			   write_term(is,
				      [ quoted(true),
					priority(699),
					partial(true)
				      ])
		       )).
test(backslash, S == "'te\\\\st'") :-
    with_output_to(string(S),
		   write_term('te\\st',
			      [ character_escapes(true),
				quoted(true)
			      ])).
test(backslash, S == "'te\\st'") :-
    with_output_to(string(S),
		   write_term('te\\st',
			      [ character_escapes(false),
				quoted(true)
			      ])).

:- end_tests(write_quoted).

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
test(variable_names, X = 'a($VAR(0),B)') :-
	with_output_to(
	    atom(X),
	    write_term(a('$VAR'(0), A),
		       [variable_names(['B'=A])])),
        A = 2.
test(variable_names, X = 'a(A,B)') :-
	with_output_to(
	    atom(X),
	    write_term(a('$VAR'(0), A),
		       [variable_names(['B'=A]), numbervars(true)])),
        A = 2.
test(variable_names) :-
	write_term('', [numbervars(true), variable_names(['XN'=V])]),
	V = 2.


:- end_tests(write_variable_names).

:- begin_tests(write_float).

% The NaN representation can vary even in the same architecture,
% but the double extracted from it by setting the exponent bits
% to 0x3ff is always a number in the range (-2,2).
% See https://github.com/SWI-Prolog/swipl-devel/issues/373
% for a more detailed explanation.
test(nan) :-
	A is nan,
	with_output_to(atom(X), write(A)),
	atom_concat(F, 'NaN', X),
	atom_number(F, Float),
	abs(Float) >= 1,
	abs(Float) < 2.

:- end_tests(write_float).

:- begin_tests(write_misc).

:- op(200, fx, fx200).

test(q_1, T == T2) :-
    T = -(0),
    term_to_atom(T, X),
    term_to_atom(T2, X).
test(q_2, T == T2) :-
    T = +(0),
    term_to_atom(T, X),
    term_to_atom(T2, X).
test(q_3, X == '+a') :-
    term_to_atom(+(a), X).
test(q_4, X == '\'/*\'') :-
    term_to_atom('/*', X).
test(q_5, X == '\'/**\'') :-
    term_to_atom('/**', X).
test(q_6, X == '*/*') :-
    term_to_atom('*/*', X).
test(q_7, X == 'p((0|a))') :-
    term_to_atom(p((0|a)), X).
test(q_8, X == 'p((a|b))') :-
    term_to_atom(p((a|b)), X).
test(q_9, X == '\'.\'') :-
    term_to_atom(., X).
test(op_1, X == '- (a,b)') :-
    term_to_atom(-((a,b)), X).
test(op_2, X == "fx200 (a,b)") :-
    context_module(M),
    term_string(fx200((a,b)), X, [module(M)]).
test(op_3, X == 'dynamic a,b') :-
    term_to_atom(dynamic((a,b)), X).
test(c_1, T2 =@= T) :-
    T = [a,b,c|T],
    term_to_atom(T, X),
    term_to_atom(@(T2,S2), X),
    maplist(call, S2).
test(s_1, X = '[(a,b)]') :-
    term_to_atom([(a,b)], X).

:- end_tests(write_misc).

write_encoding(Goal, Encoding, String) :-
	setup_call_cleanup(
	    tmp_file_stream(File, Out, [encoding(Encoding)]),
	    with_output_to(Out, Goal),
	    close(Out)),
	setup_call_cleanup(
	    open(File, read, In, [encoding(Encoding)]),
	    read_string(In, _, String),
	    close(In)),
	delete_file(File).


:- multifile
	prolog:message//1.

prolog:message(write(nan, X)) -->
	[ 'NaN is written as "~w"'-[X] ].
