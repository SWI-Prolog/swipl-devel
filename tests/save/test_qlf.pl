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
:- use_module(library(lists), [memberchk/2, member/2]).
:- use_module(library(prolog_qlfmake), []).

test_qlf :-
    run_tests([ qlf,
                qlf_staleness
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


                /*******************************
                *      IS IT OUT OF DATE?      *
                *******************************/

/* A .qlf file records a hash of the content of every source that went
   into it.  Modification times alone cannot say whether a source
   changed: a tree that arrives by checkout, copy, unpack or install
   carries times of its own, in either direction and at the resolution
   of the file system it landed on.  The time says cheaply that a file
   *may* have changed; the hash settles it.
*/

:- begin_tests(qlf_staleness).

test(the_hash_recorded_for_a_source_is_the_hash_of_the_file,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    '$qlf_sources'(Qlf, Sources),
    memberchk(source(Pl, Hash), Sources),
    Hash =\= 0,
    '$file_hash'(Pl, Hash).

%       Reinstalling a tree, or checking it out again, gives every source
%       a time of its own.  Nothing changed, so nothing is recompiled.

test(a_source_that_is_newer_but_unchanged_is_not_out_of_date,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    touch(Pl, Qlf, 10),
    \+ '$qlf_out_of_date'(Pl, Qlf, _).

test(and_one_whose_content_changed_is,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf])),
       true(Why == old)
     ]) :-
    write_source(Pl, "answer(43).\n"),
    touch(Pl, Qlf, 10),
    '$qlf_out_of_date'(Pl, Qlf, Why).

%       prolog_qlfmake allowed a second of slack, to keep a tree that was
%       copied file by file from recompiling itself.  An edit made in the
%       second the .qlf file was written went unnoticed with it.

test(a_change_within_a_second_of_the_qlf_needs_a_rebuild,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    write_source(Pl, "answer(43).\n"),
    touch(Pl, Qlf, 1),
    prolog_qlfmake:qlf_needs_rebuild(Pl).

%       The time of a file edited in the second its .qlf file was written
%       is the time of that .qlf file, at the one second many file systems
%       record.  Which is why the build asks the content and not the time.

test(and_so_does_a_change_made_in_the_same_second,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    write_source(Pl, "answer(43).\n"),
    touch(Pl, Qlf, 0),
    prolog_qlfmake:qlf_needs_rebuild(Pl).

test(and_a_touch_within_it_does_not,
     [ setup(compiled_file(Pl, Qlf)),
       cleanup(remove_files([Pl, Qlf]))
     ]) :-
    touch(Pl, Qlf, 1),
    \+ prolog_qlfmake:qlf_needs_rebuild(Pl).

%       A library that copies code into the file it is compiling -- XPCE
%       puts the methods of a class template in every class that uses one
%       -- says so through prolog:qlf_dependency/2, and the .qlf file is
%       then rebuilt when that other file changes.

test(a_dependency_the_hook_declares_is_recorded,
     [ setup(compiled_with_dependency(Pl, Qlf, Dep)),
       cleanup(remove_files([Pl, Qlf, Dep]))
     ]) :-
    '$qlf_sources'(Qlf, Sources),
    memberchk(dependency(Dep, Hash), Sources),
    Hash =\= 0,
    '$file_hash'(Dep, Hash).

test(and_a_change_to_it_needs_a_rebuild,
     [ setup(compiled_with_dependency(Pl, Qlf, Dep)),
       cleanup(remove_files([Pl, Qlf, Dep]))
     ]) :-
    \+ prolog_qlfmake:qlf_needs_rebuild(Pl),
    write_source(Dep, "% a change to what was copied\n"),
    touch(Dep, Qlf, 1),
    prolog_qlfmake:qlf_needs_rebuild(Pl).

:- end_tests(qlf_staleness).

%!  compiled_file(-PlFile, -QlfFile) is det.
%
%   A source of our own in the temporary directory and the .qlf file
%   compiled from it, so that the times of both are ours to set.

compiled_file(Pl, Qlf) :-
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, 'test_qlf_staleness.pl', Pl),
    file_name_extension(Base, pl, Pl),
    file_name_extension(Base, qlf, Qlf),
    write_source(Pl, "answer(42).\n"),
    remove_files([Qlf]),
    qcompile(Pl),
    unload_file(Pl).

%!  compiled_with_dependency(-PlFile, -QlfFile, -Dependency) is det.
%
%   As compiled_file/2, with a file the hook below says PlFile takes a
%   copy of something from.

:- dynamic
    hook_dependency/2.

:- multifile
    prolog:qlf_dependency/2.

prolog:qlf_dependency(File, Dependency) :-
    hook_dependency(File, Dependency).

compiled_with_dependency(Pl, Qlf, Dep) :-
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, 'test_qlf_dependency.pl', Dep),
    write_source(Dep, "% what the compiled file copied\n"),
    retractall(hook_dependency(_, _)),
    setup_call_cleanup(
        assertz(hook_dependency(Pl, Dep)),
        compiled_file(Pl, Qlf),
        retractall(hook_dependency(_, _))).

write_source(File, Text) :-
    setup_call_cleanup(
        open(File, write, Out),
        format(Out, '~s', [Text]),
        close(Out)).

%!  touch(+File, +Reference, +Delay) is det.
%
%   Give File a modification time Delay seconds after that of Reference,
%   so that a test does not have to wait for the clock.

touch(File, Reference, Delay) :-
    time_file(Reference, Time),
    New is Time+Delay,
    set_time_file(File, _, [modified(New)]).

remove_files(Files) :-
    forall(member(File, Files),
           catch(delete_file(File), _, true)).

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
