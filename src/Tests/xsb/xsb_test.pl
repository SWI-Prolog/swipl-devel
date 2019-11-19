/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

:- module(xsb_tests,
          [ xsb_test/3,                 % +SubDir, +File, :Goal
            xsb_cmp_results/2           % +TestFile, +OurResults
          ]).
:- use_module(library(debug)).
:- use_module(library(ordsets)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(varnumbers)).
:- use_module(library(readutil)).
:- use_module(library(dialect/xsb/source)).

xsb_test(SubDir,Base,Goal) :-
    source_file(xsb_test(_,_,_), ThisFile),
    file_directory_name(ThisFile, TestDir),
    atomic_list_concat([TestDir, SubDir, Base], /, TestFile),
    xsb_test_id(Base, TestFile, Goal).

xsb_test_id(Base, TestFile, Goal) :-
    atom_concat('xsb_', Base, Module),
    debug(xsb(test), 'Loading ~p into ~p', [TestFile, Module]),
    load_files(Module:TestFile, [if(changed), dialect(xsb)]),
    abolish_all_tables,
    with_output_to(string(New),
                   ignore(@(Module:Goal, Module))),
    atom_concat(TestFile, '_old', GoldFile),
    xsb_cmp_results(GoldFile, New).

%!  xsb_cmp_results(TestFile:atom, New:string)
%
%   Compare the collected result in New with the contents of TestFile.

xsb_cmp_results(TestFile, New) :-
    string_terms(New, Me),
    length(Me, Count),
    debug(xsb(test), 'Got ~D answer terms', [Count]),
    read_test_output(TestFile, Gold),
    compare_terms(Me, Gold).

compare_terms(Terms, Terms) :-
    !.
compare_terms(Sorted1, Sorted2) :-
    ord_intersection(Sorted1, Sorted2, Common),
    ord_subtract(Sorted1, Sorted2, Extra),
    ord_subtract(Sorted2, Sorted1, Missing),
    length(Common, CommonCount),
    length(Extra, ExtraCount),
    length(Missing, MissingCount),
    format(user_output,
           'DIFFERS: ~D common, ~D extra, ~D missing~n',
               [ CommonCount, ExtraCount, MissingCount ]),
    format(user_output, 'EXTRA:~n', []),
    forall(member(T, Extra), format(user_output, '   < ~q~n', [T])),
    format(user_output, 'MISSING:~n', []),
    forall(member(T, Missing), format(user_output, '   > ~q~n', [T])),
    fail.

read_test_output(File, Terms) :-
    read_file_to_string(File, String, []),
    string_terms(String, Terms).

string_terms(String, Terms) :-
    split_string(String, "\n", "", Lines),
    convlist(test_term, Lines, Terms0),
    sort(Terms0, Terms).

test_term("", _) :-
    !,
    fail.
test_term(Line, _) :-
    sub_string(Line, 0, _, _, "====="),
    !,
    fail.
test_term(Line, Term) :-
    catch(term_string(Term, Line, [module(xsb_tests)]),
          error(syntax_error(_),_),
          fail),
    !,
    numbervars(Term).
test_term(Line, line(Line)).

% Normally 900, but we need to process tnot x - true as tnot(x)-true
:- op(499, fy, tnot).

