/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_c_conversion,
	  [ test_c_conversion/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Test errors converting Prolog data to C

Converting Prolog text to a 0-terminated C string must not silently drop
text after an embedded 0-code.  Converting integers to C integer types
raises a domain error for negative numbers if the type is unsigned and
a representation error that includes the culprit otherwise.
*/

test_c_conversion :-
	run_tests([ nul_terminated,
		    c_integer
		  ]).

nul_atom('a\0\b').

:- begin_tests(nul_terminated).

test(getenv, error(representation_error(nul_terminated_string(A)))) :-
	nul_atom(A),
	getenv(A, _).
test(setenv, error(representation_error(nul_terminated_string(A)))) :-
	nul_atom(A),
	setenv(x, A).
test(file_name, error(representation_error(nul_terminated_string(A)))) :-
	nul_atom(A),
	exists_file(A).
test(open, error(representation_error(nul_terminated_string(A)))) :-
	nul_atom(A),
	open(A, read, _).
test(string, error(representation_error(nul_terminated_string(S)))) :-
	S = "a\0\b",
	getenv(S, _).
test(message) :-
	nul_atom(A),
	catch(getenv(A, _), E, true),
	E = error(Formal, _),
	message_to_codes(error(Formal, _), Msg),
	sub_string(Msg, 0, _, _, "Cannot represent "),
	sub_string(Msg, _, _, _, " as `nul_terminated_string'").
test(message, true(Len < 300)) :-
	length(Codes, 1000),
	maplist(=(0'x), Codes),
	append(Codes, [0], Codes1),
	atom_codes(A, Codes1),
	catch(getenv(A, _), E, true),
	E = error(Formal, _),
	message_to_codes(error(Formal, _), Msg),
	string_length(Msg, Len).

:- end_tests(nul_terminated).

:- begin_tests(c_integer).

test(negative, error(domain_error(not_less_than_zero, -5))) :-
	set_prolog_flag(table_space, -5).
test(int, error(representation_error(int(I)))) :-
	I is 1<<40,
	char_code(_, I).
test(size_t, error(representation_error(size_t(I)))) :-
	I is 1<<70,
	setarg(I, f(a), b).

:- end_tests(c_integer).

%   message_to_codes(+Term, -String)
%
%   Translate an error term to a string without the context.

message_to_codes(Term, String) :-
	message_to_codes(Term, [], Codes),
	string_codes(String, Codes).

message_to_codes(Term, _Kind, Codes) :-
	'$messages':translate_message(Term, Lines, []),
	with_output_to(codes(Codes),
		       print_message_lines(current_output, '', Lines)).
