/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2024, University of Amsterdam
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

:- module(test_string, [test_string/0]).
:- use_module(library(plunit)).
:- use_module(library(lists)).
:- use_module(library(debug)).

:- encoding(utf8).

/** <module> Test string manipulation primitives

This module is a Unit test for predicates that manage string objects.

@author	Jan Wielemaker
*/

test_string :-
	run_tests([ string,
		    string_bytes
		  ]).

:- begin_tests(string).

:- set_prolog_flag(double_quotes, string).

test(number_string, S == "42") :-
	number_string(42, S).
test(number_string, N == 42) :-
	number_string(N, "42").
test(number_string, fail) :-
	number_string(_, "42x").
test(number_string, error(type_error(number, '42'))) :-
	number_string('42', _S).
test(number_string, error(instantiation_error)) :-
	number_string(_, _).
test(string_codes, S == [97,98]) :-
	string_codes("ab", S).
test(string_codes, Out == Codes) :-
	numlist(0, 2000, Codes),		% verify 0-bytes and Unicode
	string_codes(S, Codes),
	string_length(S, 2001),
	string_codes(S, Out).
test(string_chars, S == [a,b]) :-
	string_chars("ab", S).
test(split_string, L == ["a", "b", "c", "d"]) :-
	split_string("a.b.c.d", ".", "", L).
test(split_string, L == ["SWI-Prolog", "7.0"]) :-
	split_string("SWI-Prolog, 7.0", ",", " ", L).
test(split_string, L == ["SWI-Prolog"]) :-
	split_string("  SWI-Prolog  ", "", "\s\t\n", L).
test(split_string, L == [""]) :-
	split_string(" ", "", " ", L).
test(string_lower, L == "abc") :-
	string_lower("aBc", L).
test(string_upper, L == "ABC") :-
	string_upper("aBc", L).
test(string_upper) :-		% Issue #1262 (iso -> wide)
	string_upper("wh\u00ff", Upr),
	assertion((   Upr == "WH\u0178" % If locale supports Unicode
		  ;   Upr == "WH\u00ff"	% Otherwise
		  )).

:- end_tests(string).

:- begin_tests(string_bytes).

test(aap, L == [97, 97, 112]) :-
	string_bytes(aap, L, utf8).
test(aap, S == "aap") :-
	string_bytes(S, [97, 97, 112], utf8).
test(aap, L == [0, 97, 0, 97, 0, 112]) :-
	string_bytes(aap, L, utf16be).
test(aap, S == "aap") :-
	string_bytes(S, [0, 97, 0, 97, 0, 112], utf16be).
test(aap, L == [97, 0, 97, 0, 112, 0]) :-
	string_bytes(aap, L, utf16le).
test(aap, S == "aap") :-
	string_bytes(S, [97, 0, 97, 0, 112, 0], utf16le).
test(hello, L == [228,187,138,230,151,165,227,129,175]) :-
	string_bytes(今日は, L, utf8).		% japanese for hello, good day
test(hello, S == "今日は") :-
	string_bytes(S, [228,187,138,230,151,165,227,129,175], utf8).
test(hello, L == [202,78,229,101,111,48]) :-
	string_bytes(今日は, L, utf16le).
test(hello, S == "今日は") :-
	string_bytes(S, [202,78,229,101,111,48], utf16le).
test(hello, error(syntax_error(incomplete_multibyte_sequence))) :-
	string_bytes(_, [202,78,229,101,111], utf16le).
test(hello, error(representation_error(encoding))) :-
	string_bytes(今日は, _, iso_latin_1).

:- end_tests(string_bytes).
