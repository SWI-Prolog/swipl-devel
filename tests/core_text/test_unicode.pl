/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2023, SWI-Prolog Solutions b.v.
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

:- module(test_unicode,
          [ test_unicode/0
          ]).
:- use_module(library(plunit)).
:- encoding(utf8).

/** <module> Unicode parsing tests

*/

test_unicode :-
    run_tests([ numbers,
		unicode_preds,
		utf8_decode
	      ]).

:- begin_tests(numbers).

test(read, error(syntax_error(illegal_character))) :-
    term_string(_, "٠١٢٣٤٥٦٧٨٩").
test(number_codes, N == 0123456789) :-
    number_codes(N, `٠١٢٣٤٥٦٧٨٩`).
test(atom_number, N == 0123456789) :-
    atom_number('٠١٢٣٤٥٦٧٨٩', N).
test(string_number, N == 0123456789) :-
    number_string(N, "٠١٢٣٤٥٦٧٨٩").
test(string_number, N == 12.3456789) :-
    number_string(N, "١٢.٣٤٥٦٧٨٩").
test(string_number, N == -12.34567e89) :-
    number_string(N, "-١٢.٣٤٥٦٧e٨٩").
test(string_number, N == -12.34567e-89) :-
    number_string(N, "-١٢.٣٤٥٦٧e-٨٩").

:- end_tests(numbers).

:- begin_tests(unicode_preds).

test(atom_length, Len == 1) :-
    atom_length('\U0001F600', Len).
test(string_length, Len == 1) :-
    atom_length('\U0001F600', Len).

:- end_tests(unicode_preds).

:- begin_tests(utf8_decode).

%   decode(+Bytes, -Codes)
%
%   Decode Bytes as UTF-8 from a stream.  Warnings for illegal input are
%   suppressed.

decode(Bytes, Codes) :-
    tmp_file_stream(binary, File, Out),
    call_cleanup(
        ( maplist(put_byte(Out), Bytes),
          close(Out),
          setup_call_cleanup(
              open(File, read, In, [encoding(utf8)]),
              with_quiet_warnings(read_codes(In, Codes)),
              close(In))
        ),
        delete_file(File)).

with_quiet_warnings(Goal) :-
    setup_call_cleanup(
        asserta((user:message_hook(_, warning, _) :- true), Ref),
        Goal,
        erase(Ref)).

read_codes(In, Codes) :-
    get_code(In, C0),
    read_codes(C0, In, Codes).

read_codes(-1, _, []) :- !.
read_codes(C, In, [C|T]) :-
    get_code(In, C1),
    read_codes(C1, In, T).

test(valid, Codes == [0x41, 0xE9, 0x20AC, 0x1F600]) :-
    decode([0x41, 0xC3,0xA9, 0xE2,0x82,0xAC, 0xF0,0x9F,0x98,0x80], Codes).
test(overlong, Codes == [0xFFFD, 0xFFFD, 0xFFFD, 0x41]) :-
    decode([0xC0,0xAE, 0xC0,0xAF, 0xC0,0x80, 0x41], Codes).
test(overlong3, Codes == [0xFFFD, 0x41]) :-
    decode([0xE0,0x80,0xAF, 0x41], Codes).
test(too_large, Codes == [0xFFFD, 0x41]) :-
    decode([0xF4,0x90,0x80,0x80, 0x41], Codes).
test(five_byte, Codes == [0xFFFD,0xFFFD,0xFFFD,0xFFFD,0xFFFD, 0x41]) :-
    decode([0xF8,0x88,0x80,0x80,0x80, 0x41], Codes).
test(truncated, Codes == [0x41, 0xFFFD]) :-
    decode([0x41, 0xE2,0x82], Codes).

:- end_tests(utf8_decode).
