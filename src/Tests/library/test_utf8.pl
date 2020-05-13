/*  Part of SWI-Prolog

    Author:        Jan Wielemaker, Eric G. Taucher
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2006-2020, University of Amsterdam
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

:- module(test_utf8,
	  [ test_utf8/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_utf8 :-
    run_tests([utf8]).

:- begin_tests(utf8).

unicode_utf8_test_case_generator(0x00    ,[               0x00]).
unicode_utf8_test_case_generator(0'a     ,[               0'a ]).
unicode_utf8_test_case_generator(0'z     ,[               0'z ]).
unicode_utf8_test_case_generator(0'A     ,[               0'A ]).
unicode_utf8_test_case_generator(0'Z     ,[               0'Z ]).
unicode_utf8_test_case_generator(0'0     ,[               0'0 ]).
unicode_utf8_test_case_generator(0'9     ,[               0'9 ]).
unicode_utf8_test_case_generator(0x7F    ,[               0x7F]).
unicode_utf8_test_case_generator(0x0080  ,[          0xC2,0x80]).
unicode_utf8_test_case_generator(0xC0    ,[          0xC3,0x80]).
unicode_utf8_test_case_generator(0x07FF  ,[          0xDF,0xBF]).
unicode_utf8_test_case_generator(0x0800  ,[     0xE0,0xA0,0x80]).
unicode_utf8_test_case_generator(0xFFFF  ,[     0xEF,0xBF,0xBF]).
unicode_utf8_test_case_generator(0x010000,[0xF0,0x90,0x80,0x80]).
unicode_utf8_test_case_generator(0x10FFFF,[0xF4,0x8F,0xBF,0xBF]).

test(unicode_code_point_to_utf8,[forall(unicode_utf8_test_case_generator(Unicode_code_point,Expected_UTF8_encoded))]) :-
    DCG = utf8_codes([Unicode_code_point]),
    phrase(DCG,UTF8_encoded,Hole),
    assertion( var(Hole) ),
    Hole = [],
    assertion( UTF8_encoded == Expected_UTF8_encoded ).

test(utf8_to_unicode_code_point,[forall(unicode_utf8_test_case_generator(Expected_unicode_code_point,UTF8_encoded))]) :-
    DCG = utf8_codes([Unicode_code_point|Hole]),
    phrase(DCG,UTF8_encoded,Hole),
    assertion( Hole == [] ),
    assertion( Unicode_code_point == Expected_unicode_code_point ).

unicode_string_utf8_test_case_generator("a"         ,[            97]).
unicode_string_utf8_test_case_generator("a"         ,[           0'a]).
unicode_string_utf8_test_case_generator("a"         ,[          0x61]).
unicode_string_utf8_test_case_generator("a"         ,[         16'61]).
unicode_string_utf8_test_case_generator("a"         ,[    0b01100001]).
unicode_string_utf8_test_case_generator("a"         ,[    2'01100001]).
unicode_string_utf8_test_case_generator("a"         ,[         0o141]).
unicode_string_utf8_test_case_generator("a"         ,[         8'141]).
unicode_string_utf8_test_case_generator("\x61"      ,[          0'a ]).
unicode_string_utf8_test_case_generator("\u0061"    ,[          0'a ]).
unicode_string_utf8_test_case_generator("\U00000061",[          0'a ]).
unicode_string_utf8_test_case_generator("\141"      ,[          0'a ]).
unicode_string_utf8_test_case_generator("\x00"      ,[          0x00]).
unicode_string_utf8_test_case_generator("\u0000"    ,[          0x00]).
unicode_string_utf8_test_case_generator("\U00000000",[          0x00]).
unicode_string_utf8_test_case_generator("\x7F"      ,[          0x7F]).
unicode_string_utf8_test_case_generator("\u007F"    ,[          0x7F]).
unicode_string_utf8_test_case_generator("\U0000007F",[          0x7F]).
unicode_string_utf8_test_case_generator("\x80"      ,[     0xC2,0x80]).
unicode_string_utf8_test_case_generator("\u0080"    ,[     0xC2,0x80]).
unicode_string_utf8_test_case_generator("\U00000080",[     0xC2,0x80]).
unicode_string_utf8_test_case_generator("\x07FF"    ,[     0xDF,0xBF]).
unicode_string_utf8_test_case_generator("\u07FF"    ,[     0xDF,0xBF]).
unicode_string_utf8_test_case_generator("\U000007FF",[     0xDF,0xBF]).
unicode_string_utf8_test_case_generator("\x0800"    ,[0xE0,0xA0,0x80]).
unicode_string_utf8_test_case_generator("\u0800"    ,[0xE0,0xA0,0x80]).
unicode_string_utf8_test_case_generator("\U00000800",[0xE0,0xA0,0x80]).
unicode_string_utf8_test_case_generator("\xFFFF"    ,[0xEF,0xBF,0xBF]).
unicode_string_utf8_test_case_generator("\uFFFF"    ,[0xEF,0xBF,0xBF]).
unicode_string_utf8_test_case_generator("\U0000FFFF",[0xEF,0xBF,0xBF]).
unicode_string_utf8_test_case_generator("\u0000azAZ09\u007F\u0080\u07FF\u0800\uFFFF",[0x00,0'a,0'z,0'A,0'Z,0'0,0'9,0x7F,0xC2,0x80,0xDF,0xBF,0xE0,0xA0,0x80,0xEF,0xBF,0xBF]).
unicode_string_utf8_test_case_generator(" azAZ09~\u00C2\u00A1\u00C3\u00BF\u00C4\u20AC\u00E2\u203A\u00B0",
                                        [  32,  97, 122,  65,  90,  48,  57, 126, 195, 130, 194, 161, 195,
                                           131, 194, 191, 195, 132, 226, 130, 172, 195, 162, 226, 128, 186,
                                           194, 176]).

test(unicode_string_to_utf8,[forall(unicode_string_utf8_test_case_generator(Unicode_string,Expected_UTF8_encoded))]) :-
    string_codes(Unicode_string,Unicode_codes),
    DCG = utf8_codes(Unicode_codes),
    phrase(DCG,UTF8_encoded,Hole),
    assertion( var(Hole) ),
    Hole = [],
    assertion( UTF8_encoded == Expected_UTF8_encoded ).

test(utf8_to_unicode_string,[forall(unicode_string_utf8_test_case_generator(Expected_unicode_string,UTF8_encoded))]) :-
    DCG = utf8_codes(Unicode_codes),
    phrase(DCG,UTF8_encoded,Hole),
    assertion( Hole == [] ),
    string_codes(Unicode_string,Unicode_codes),
    assertion( Unicode_string == Expected_unicode_string ).

:- end_tests(utf8).
