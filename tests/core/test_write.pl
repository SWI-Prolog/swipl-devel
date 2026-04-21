/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2011-2025, University of Amsterdam
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
:- use_module(library(apply)).
:- encoding(utf8).

/** <module> Test write_term/2 and friends.

*/

test_write :-
	run_tests([ portray,
		    write_op,
		    write_canonical,
		    write_quoted,
		    write_variable_names,
		    write_float,
		    write_misc,
		    max_text,
		    write_size
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

:- begin_tests(max_text).

test(string, Out == "\"hello\"") :-
	max_text(5, "hello", Out, [quoted(true)]).
test(string, Out == "\"hel…rld\"") :-
	max_text(6, "hello world", Out, [quoted(true), truncated(Bool)]),
	assertion(Bool == true).
test(string, Out == "\"…\"") :-
	max_text(0, "hello world", Out, [quoted(true)]).
test(string, Out == "\"h…\"") :-
	max_text(1, "hello world", Out, [quoted(true)]).
test(string, Out == "\"h…d\"") :-
	max_text(2, "hello world", Out, [quoted(true)]).
test(string, Out == "\"hel…ιος\"") :-
	max_text(6, "hello υφήλιος", Out, [quoted(true)]).
test(atom, Out == "'hel…ιος'") :-
	max_text(6, 'hello υφήλιος', Out, [quoted(true)]).
test(atom, Out == "hello_υφήλιος") :-
	max_text(20, hello_υφήλιος, Out, [quoted(true)]).
test(atom, Out == "'hel…ιος'") :-
	max_text(6, hello_υφήλιος, Out, [quoted(true)]).
test(atom, Out == "'\\nhe…ιος'") :-
	max_text(6, '\nhello_υφήλιος', Out, [quoted(true)]).
test(atom, Out == "hel…ιος") :-
	max_text(6, hello_υφήλιος, Out, [quoted(false)]).

max_text(Max, Term, String, Options) :-
	with_output_to(string(String),
		       write_term(Term, [max_text(Max)|Options])).

:- end_tests(max_text).

:- begin_tests(write_size).

% Basic width / height — a single-line term is 1 line tall.
test(atom, W-H == 5-1) :-
	write_size(hello, W, H, []).
test(compound, W-H == 8-1) :-
	write_size(foo(bar), W, H, []).
test(empty_atom, W-H == 2-1) :-				% quoted ''
	write_size('', W, H, [quoted(true)]).
test(empty_atom_unquoted, W-H == 0-0) :-		% writes nothing
	write_size('', W, H, [quoted(false)]).
test(trailing_newline, W-H == 1-2) :-			% caret landed on line 2
	write_size('a\n', W, H, [quoted(false)]).

% Width / Height reflect embedded newlines in unquoted atoms.
test(newline, W-H == 1-2) :-				% two lines: "a", "b"
	write_size('a\nb', W, H, [quoted(false)]).
test(newlines, W-H == 2-3) :-				% "a", "bb", "c"
	write_size('a\nbb\nc', W, H, [quoted(false)]).

% max_width: inclusive — exact fit succeeds, strict overflow fails.
test(max_width_exact) :-
	write_size(hello, 5, 1, [max_width(5)]).
test(max_width_overflow, fail) :-
	write_size(hello, _, _, [max_width(4)]).
test(max_width_loose, W-H == 5-1) :-
	write_size(hello, W, H, [max_width(100)]).

% max_height: inclusive count of lines.
test(max_height_single) :-
	write_size(hello, 5, 1, [max_height(1)]).
test(max_height_exact) :-
	write_size('a\nb', 1, 2, [quoted(false), max_height(2)]).
test(max_height_overflow, fail) :-
	write_size('a\nb', _, _, [quoted(false), max_height(1)]).

% Unicode combining marks count as 0 columns.
test(combining, W-H == 1-1) :-				% à NFD: a + U+0300
	atom_codes(A, [0'a, 0x0300]),
	write_size(A, W, H, [quoted(false)]).
test(combining_stack, W-H == 1-1) :-			% u + 2 combiners
	atom_codes(A, [0'u, 0x0308, 0x0306]),
	write_size(A, W, H, [quoted(false)]).

% Wide / double-width characters count as 2 columns.
test(cjk, W-H == 2-1) :-
	write_size('中', W, H, [quoted(false)]).
test(cjk_pair, W-H == 4-1) :-
	write_size('中国', W, H, [quoted(false)]).
test(emoji_vs16, W-H == 2-1) :-				% 🤩 + VS-16
	atom_codes(A, [0x1F929, 0xFE0F]),
	write_size(A, W, H, [quoted(false)]).

% Mixed: ASCII + wide.
test(mixed, W-H == 7-1) :-				% 'hello中'
	write_size('hello中', W, H, [quoted(false)]).

% Long input — the stream buffer holds 100 wchar_t entries, so
% writing a 300-char atom exercises three Swrite_lss chunks.  Also
% pin down that max_width aborts mid-write instead of walking the
% whole atom.
test(long, W-H == 300-1) :-
	length(Cs, 300), maplist(=(0'x), Cs),
	atom_codes(A, Cs),
	write_size(A, W, H, [quoted(false)]).
test(long_max_exact, W-H == 300-1) :-
	length(Cs, 300), maplist(=(0'x), Cs),
	atom_codes(A, Cs),
	write_size(A, W, H, [quoted(false), max_width(300)]).
test(long_max_overflow, fail) :-
	length(Cs, 300), maplist(=(0'x), Cs),
	atom_codes(A, Cs),
	write_size(A, _, _, [quoted(false), max_width(299)]).
test(long_multiline, W-H == 40-12) :-			% 12 × 40 'y' lines
	length(Codes, 40), maplist(=(0'y), Codes),
	atom_codes(LineA, Codes),
	length(Lines, 12), maplist(=(LineA), Lines),
	atomic_list_concat(Lines, '\n', Big),
	write_size(Big, W, H, [quoted(false)]).
test(long_multiline_max_height_overflow, fail) :-
	length(Codes, 40), maplist(=(0'y), Codes),
	atom_codes(LineA, Codes),
	length(Lines, 12), maplist(=(LineA), Lines),
	atomic_list_concat(Lines, '\n', Big),
	write_size(Big, _, _, [quoted(false), max_height(11)]).

% UTF-16 surrogate pair across the buffer boundary (Windows only,
% SIZEOF_WCHAR_T == 2).  99 ASCII chars + one non-BMP emoji (🤩
% U+1F929) produce 101 wchar_t entries on Windows, so the buffer
% flush at 100 wchar_ts lands between the lead and trail surrogates.
% Swrite_lss must stash the lead and prepend it to the next chunk;
% the computed width/height must still match.  On Linux
% (SIZEOF_WCHAR_T == 4) the emoji is a single wchar_t, so the same
% test doubles as a plain long-input check.
test(surrogate_split, W-H == 101-1) :-
	length(Xs, 99), maplist(=(0'x), Xs),
	append(Xs, [0x1F929], Codes),
	atom_codes(A, Codes),
	write_size(A, W, H, [quoted(false)]).
test(surrogate_split_max_exact, W-H == 101-1) :-
	length(Xs, 99), maplist(=(0'x), Xs),
	append(Xs, [0x1F929], Codes),
	atom_codes(A, Codes),
	write_size(A, W, H, [quoted(false), max_width(101)]).
test(surrogate_split_max_overflow, fail) :-
	length(Xs, 99), maplist(=(0'x), Xs),
	append(Xs, [0x1F929], Codes),
	atom_codes(A, Codes),
	write_size(A, _, _, [quoted(false), max_width(100)]).

:- end_tests(write_size).

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
