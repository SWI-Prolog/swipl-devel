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


:- module(test_load_context,
          [ test_load_context/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(lists), [member/2]).

/** <module> Loading a file records the module it was loaded from

source_file_property(File, load_context(Module, Location, Options)) is
what make/0 uses to reload a file the way it was loaded, and what
prolog:qlf_dependency/2 of library(pce_expansion) walks to find the .qlf
file a class template was copied into.  Recording it was left to the
loader of the multi-threaded system, so a single threaded system had no
load context for any file at all.

Run with:

    swipl -g test_load_context -t halt tests/compile/test_load_context.pl
*/

test_load_context :-
    run_tests([ load_context ]).

:- begin_tests(load_context).

test(a_consulted_file_is_loaded_from_the_module_that_consults_it,
     [ setup(write_files(Parent, Part)),
       cleanup(remove_files([Parent, Part]))
     ]) :-
    ensure_loaded(user:Parent),
    once(source_file_property(Part, load_context(Module, Location:_, _Options))),
    unload_file(Parent),
    unload_file(Part),
    assertion(Module == test_load_context_parent),
    % same_file is more robust than Location = Parent (Win 8.3 filenames)
    assertion(same_file(Location, Parent)). 

:- end_tests(load_context).

%!  write_files(-Parent, -Part) is det.
%
%   A module file that consults a file which is not one.  Both are in the
%   temporary directory: the test loads them.

write_files(Parent, Part) :-
    tmp_file_name('test_load_context_parent.pl', Parent),
    tmp_file_name('test_load_context_part.pl', Part),
    write_file(Part, "part_of_the_context(true).\n"),
    format(string(ParentText),
           ":- module(test_load_context_parent, []).~n\c
            :- consult('~w').~n", [Part]),
    write_file(Parent, ParentText).

tmp_file_name(Base, Path) :-
    current_prolog_flag(tmp_dir, Tmp),
    directory_file_path(Tmp, Base, Path).

write_file(File, Text) :-
    setup_call_cleanup(
        open(File, write, Out),
        write(Out, Text),
        close(Out)).

remove_files(Files) :-
    forall(member(File, Files),
           catch(delete_file(File), _, true)).
