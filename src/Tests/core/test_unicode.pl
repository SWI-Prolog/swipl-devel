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
		unicode_preds
	      ]).

:- begin_tests(numbers).

test(read, N == 0123456789) :-
    term_string(N, ٠١٢٣٤٥٦٧٨٩).
:- if(current_prolog_flag(bounded, false)).
test(read, N == 0123456789r23) :-
    term_string(N, ٠١٢٣٤٥٦٧٨٩r٢٣).
:- endif.
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
