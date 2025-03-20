/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2025, SWI-Prolog Solutions b.v.
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

:- module(app_app, []).

:- use_module(library(main)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Pos, Options),
    app(Pos, Options),
    !.
main(_) :-
    argv_usage(debug).

opt_type(long, long, boolean).
opt_type(l,    long, boolean).
opt_help(long, "Long format").
opt_help(help(usage), " list -l]").

app([list], Options) :-
    findall(Name-File, current_app(Name, File), Apps),
    keysort(Apps, Sorted),
    group_pairs_by_key(Sorted, Keyed),
    maplist(list_app(Options), Keyed).

list_app(Options, App-[File|Masked]) :-
    option(long(true), Options),
    !,
    ansi_format(bold, '~w', [App]),
    ansi_format(code, '~t~20|~w~n', [File]),
    forall(member(M, Masked),
           ansi_format(warning, '~t~20|~w (masked)~n', [M])).
list_app(_Options, App-[_]) :-
    format('~w~n', [App]).

current_app(Name, File) :-
    absolute_file_name(app(.), Dir,
                       [ file_type(directory),
                         solutions(all)
                       ]),
    findall(Ext, prolog_file_type(Ext, prolog), Exts),
    atomics_to_string(Exts,",",ExtPattern),
    format(string(Pattern), '~w/*.{~w}', [Dir, ExtPattern]),
    expand_file_name(Pattern, Files),
    member(File, Files),
    file_base_name(File, Base),
    file_name_extension(Name, _, Base).


