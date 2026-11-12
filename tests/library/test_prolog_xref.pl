/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
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


:- module(test_prolog_xref,
          [ test_prolog_xref/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(prolog_xref)).
:- use_module(library(prolog_source)).
:- use_module(library(lists)).

test_prolog_xref :-
    run_tests([ prolog_canonical_source,
                xref_defined_class
              ]).

:- dynamic data_dir/1.

:- prolog_load_context(directory, Dir),
   directory_file_path(Dir, xref, DataDir),
   asserta(data_dir(DataDir)).

data_file(Name, Path) :-
    data_dir(Dir),
    directory_file_path(Dir, Name, Path).

%   canonical_relative(+File, -Id)
%
%   Id is the canonical source of File, named relative to its
%   directory.

canonical_relative(File, Id) :-
    data_dir(Dir),
    setup_call_cleanup(
        working_directory(Old, Dir),
        prolog_canonical_source(File, Id),
        working_directory(_, Old)).

:- begin_tests(prolog_canonical_source).

test(alias_of_loaded_file, Id == Abs) :-
    % library(lists) is loaded; the alias must not be its own id
    prolog_canonical_source(library('lists.pl'), Id),
    absolute_file_name(library(lists), Abs,
                       [file_type(prolog), access(read)]).
test(relative_name_of_loaded_file, [Before-After == Abs-Abs]) :-
    % The id of a file must not change when the file is loaded
    data_file('xref_loaded.pl', Abs),
    canonical_relative('xref_loaded.pl', Before),
    use_module(Abs),
    canonical_relative('xref_loaded.pl', After).

:- end_tests(prolog_canonical_source).

:- begin_tests(xref_defined_class).

test(enumerate, [ condition(current_prolog_flag(threads, true)),
                  Classes == [xref_test_class_a, xref_test_class_b]
                ]) :-
    data_file('xref_classes.pl', File),
    thread_self(Me),
    thread_create(( defined_classes(File, Classes0),
                    thread_send_message(Me, xref_classes(Classes0))
                  ), Id),
    thread_join(Id, true),
    thread_get_message(xref_classes(Classes)).

%   Disable autoloading, as reading the directive would otherwise
%   autoload xpce.  The flag is thread-local.

defined_classes(File, Classes) :-
    set_prolog_flag(autoload, false),
    setup_call_cleanup(
        xref_source(File, [silent(true)]),
        findall(C, xref_defined_class(File, C, local(_,_,_)), Classes0),
        xref_clean(File)),
    msort(Classes0, Classes).

:- end_tests(xref_defined_class).
