/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2024, University of Amsterdam
                              VU University Amsterdam
		              CWI, Amsterdam
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

:- module(test_qlf,
          [ test_qlf/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(debug), [assertion/1, debug/3]).
:- use_module(library(apply), [maplist/3, maplist/2]).
:- use_module(library(prolog_code), [pi_head/2]).

test_qlf :-
    run_tests([ qlf
              ]).

find_me.
file_path(File, Path) :-
    source_file(find_me, Here),
    file_directory_name(Here, Dir),
    directory_file_path(Dir, File, Path).

:- begin_tests(qlf).

test(unicode,
     [ Found =@= Expected,
       setup(test_files(unicode, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [data(_)],
             Expected, Found),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(h_integer,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [head(_)],
             Expected, Found),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(b_integer,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [body(_)],
             Expected, Found),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(expr,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [expr(_)],
             Expected, Found,
             [ optimise(true) ]),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(cmp,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [cmp(_)],
             Expected, Found,
             [ optimise(true) ]),
    assertion((Expected = [List], maplist(==(0), List))),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(add_fc,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [add(_)],
             Expected, Found,
             [ optimise(true) ]),
    debug(qlf(result), '~q~n~q', [Expected, Found]).
test(rat,
     [ Found =@= Expected,
       setup(test_files(integers, Prolog, Qlf)),
       cleanup(catch(delete_file(Qlf), _, true))
     ]) :-
    qlf_trip(Prolog,
             Qlf,
             [rat(_)],
             Expected, Found,
             [ optimise(true) ]),
    debug(qlf(result), '~q~n~q', [Expected, Found]).

:- end_tests(qlf).

test_files(Spec, Prolog, Qlf) :-
    atomic_list_concat([input,Spec], /, RelFile),
    file_path(RelFile, Prolog),
    file_base_name(Prolog, Base),
    file_name_extension(Base, qlf, QlfFile),
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, QlfFile, Qlf).

%!  qlf_trip(+Prolog, +Qlf, :Goals, -Expected, -Found) is det.
%!  qlf_trip(+Prolog, +Qlf, :Goals, -Expected, -Found, +Options) is det.
%
%   Load and qcompile the file  Input   from  the directory `input`, run
%   call(Goal, Expected) to get the expected data, unload the file, load
%   the generated `.qlf` file and call call(Goal, Found.
%
%   Note that ['$qlf'(Qlf)] is an undocumented way   to save the file in
%   an explicit location.

qlf_trip(Prolog, Qlf, Goals, Expected, Found) :-
    qlf_trip(Prolog, Qlf, Goals, Expected, Found, []).

qlf_trip(Prolog, Qlf, Goals, Expected, Found, Options) :-
    catch(delete_file(Qlf), _, true),
    load_files(Prolog, ['$qlf'(Qlf)|Options]),
    run(Goals, Expected),
    unload_file(Prolog),
    assert_unloaded(Goals),
    consult(Qlf),
    run(Goals, Found).

run(Goals, Result) :-
    maplist(run1, Goals, Result).

run1(Goal, Result) :-
    term_variables(Goal, Vars),
    (   Vars = [Templ]
    ->  true
    ;   Templ =.. [v|Vars]
    ),
    findall(Templ, Goal, Result).

assert_unloaded(Goals) :-
    maplist(assert_unloaded1, Goals).

assert_unloaded1(Head) :-
    pi_head(PI, Head),
    assertion(\+ current_predicate(PI)).
