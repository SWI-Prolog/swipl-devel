/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2020, VU University Amsterdam
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

:- module(test_glob,
          [ test_glob/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(ordsets)).
:- use_module(library(random)).

test_glob :-
    run_tests([ glob_match,
                glob_expand
              ]).

:- begin_tests(glob_match).

test(glob, true) :- wildcard_match('', '').
test(glob, fail) :- wildcard_match('', 'a').
test(glob, fail) :- wildcard_match('a', '').
test(glob, true) :- wildcard_match('a?', 'aX').
test(glob, true) :- wildcard_match('a[xyz]', 'ax').
test(glob, true) :- wildcard_match('a[xy\\]]', 'a]').
test(glob, true) :- wildcard_match('a\\', 'a\\').
test(glob, true) :- wildcard_match('a\\a', 'aa').
test(glob, true) :- wildcard_match('a\\[a', 'a[a').
test(glob, true) :- wildcard_match('a[x-z]b', 'ayb').
test(glob, true) :- wildcard_match('a[x-]b', 'axb').
test(glob, true) :- wildcard_match('a[-x]b', 'axb').
test(glob, true) :- wildcard_match('a{[-x],c}b', 'acb').
test(glob, true) :- wildcard_match('a{[-x],c,}b', 'ab').
test(glob, true) :- wildcard_match('a{[x-z],c,}b', 'ayb').
test(glob, true) :- wildcard_match([65], 'A').
test(glob, true) :- wildcard_match('a[\u0400-\u0450]b', 'a\u0425b').
test(glob, true) :- wildcard_match(a, 'A', [case_sensitive(false)]).
test(glob, fail) :- wildcard_match(a, 'A', [case_sensitive(true)]).
test(glob, true) :- wildcard_match('A', a, [case_sensitive(false)]).
test(glob, fail) :- wildcard_match('A', a, [case_sensitive(true)]).
test(glob, fail) :- wildcard_match(a, 'A').

% error cases
test(glob, error(type_error(character_code,0x110000))) :-
    wildcard_match([0x110000], 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se{xx', 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se{xx\\', 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se{xx\u0440', 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se{xx[z', 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se[xy', 'A').
test(glob, error(syntax_error(_))) :-
    wildcard_match('se[xy\\', 'A').

:- end_tests(glob_match).

:- begin_tests(glob_expand).

base(Base) :-
    tmp_file(glob, Base).

test_expand(Paths, Pattern) :-
    setup_call_cleanup(
        base(Base),
        test_expand(Base, Paths, Pattern),
        delete_directory_and_contents(Base)).

test_expand(Base, Paths, Pattern) :-
    maplist(create_path(Base), Paths, Files),
    atomic_list_concat([Base, Pattern], /, Pattern1),
    expand_file_name(Pattern1, FileMatches),
    include(wildcard_match(Pattern1), Files, PatternMatches),
    same_set(PatternMatches, FileMatches).

create_path(Base, Path, File) :-
    atomic_list_concat([Base, Path], /, File),
    create_file(File).

create_file(File) :-
    file_directory_name(File, Dir),
    make_directory_path(Dir),
    call_cleanup(open(File, write, Out),
                 close(Out)).

same_set(S1, S2) :-
    msort(S1, SS1),
    msort(S2, SS2),
    (   SS1 == SS2
    ->  true
    ;   ord_subtract(SS1, SS2, Missing),
        format(user_error, "Missing: ~p~n", [Missing]),
        ord_subtract(SS2, SS1, Extra),
        format(user_error, "Extra: ~p~n", [Extra]),
        assertion(SS1 == SS2)
    ).

can_represent(Code) :-
    catch(setup_call_cleanup(
              open_null_stream(S),
              ( set_stream(S, encoding(text)),
                put_code(S, Code)
              ),
              close(S)),
          error(_,_),
          fail).

test(glob) :-
    test_expand(['x.pl'], '*.pl').
:- if(can_represent(0x440)).
test(cyrillic) :-
    unicode_name("", "", 16, File),
    test_expand([File], File).
test(cyrillic) :-
    unicode_name("", ".u", 16, File1),
    unicode_name("", ".u", 32, File2),
    test_expand([File1,File2], '*.u').
test(cyrillic) :-
    unicode_name("", "/x.pl", 16, File),
    test_expand([File], '*/x.pl').
:- endif.

:- end_tests(glob_expand).

unicode_name(Pre, Post, Len, Name) :-
    length(S, Len),
    maplist(unicode_char, S),
    string_chars(SS, S),
    atomic_list_concat([Pre,SS,Post], Name).

unicode_char(C) :-
    random_between(0x400, 0x4ff, C).
