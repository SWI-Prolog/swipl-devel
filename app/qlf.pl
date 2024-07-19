/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(app_qlf, []).

:- use_module(library(main)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/high_order)).
:- use_module(library(option)).
:- use_module(library(ansi_term)).

:- initialization(main, main).

main([Argv1|Argv]) :-
    qlf(Argv1, Argv),
    !.
main(_) :-
    usage,
    halt(1).

qlf(compile, Argv) =>
    argv_deps_preload(Argv, Deps, Preload, Argv1),
    (   memberchk('--expect-deps', Argv)
    ->  Options = [expect_deps(Deps)|Options1]
    ;   Options = Options1
    ),
    (   memberchk('--preload', Argv)
    ->  Options1 = [preload(Preload)|Options2]
    ;   Options1 = Options2
    ),
    argv_options(qlf_compile:Argv1, Files, Options2),
    maplist(expand_compile_opt, Options, RunOptions),
    cli_qlf_compile(Files, RunOptions).
qlf(update, Argv) =>
    argv_options(qlf_update:Argv, Files, Options),
    cli_qlf_update(Files, Options).
qlf(info, Argv) =>
    argv_options(qlf_info:Argv, [File], Options),
    cli_qlf_info(File, Options).
qlf(list, Argv) =>
    argv_options(qlf_list:Argv, Pos, Options),
    cli_qlf_list(Pos, Options).
qlf(clean, Argv) =>
    argv_options(qlf_clean:Argv, Pos, Options),
    cli_qlf_clean(Pos, Options).
qlf(help, [Command]) =>
    qlf_command(Command, _),
    atom_concat(qlf_, Command, Module),
    argv_usage(Module:debug).
qlf(_, _) =>
    usage,
    halt(1).

qlf_compile:opt_type(include,   include,   boolean).
qlf_compile:opt_help(help(usage),
                     " compile [--include] file ...").
qlf_compile:opt_help(help(header),
                     [ansi(bold, "Compile Prolog into .qlf files.", [])]).
qlf_compile:opt_help(include,
                     "Include other user files into .qlf file").

qlf_update:opt_type(include,   include,   boolean).
qlf_update:opt_type(Flag, Opt, Type) :- qlf_list:opt_type(Flag, Opt, Type).

qlf_update:opt_help(help(header),
                    [ansi(bold, "Recompile outdated .qlf files.", [])]).
qlf_update:opt_help(help(usage),
                     " update [option ...] file-or-directory ...").
qlf_update:opt_help(include,
                    "Include other user files into .qlf file").
qlf_update:opt_help(Opt, Message) :-
    qlf_list:opt_help(Opt, Message),
    atom(Opt).


qlf_info:opt_type(source,    source,    boolean).
qlf_info:opt_type(s,         source,    boolean).
qlf_info:opt_type(version,   version,   boolean).
qlf_info:opt_type(v,         version,   boolean).

qlf_info:opt_help(help(usage),
                  " info [option ...] file").
qlf_info:opt_help(source,
                  "List the source files from which this QLF file was created").
qlf_info:opt_help(version,
                  "List version information about QLF file").

qlf_list:opt_type(recursive, recursive, boolean).
qlf_list:opt_type(r,         recursive, boolean).
qlf_list:opt_type(all,       all,       boolean).
qlf_list:opt_type(a,         all,       boolean).

qlf_list:opt_help(help(header),
                   [ansi(bold, "List .qlf files and their status.", [])]).
qlf_list:opt_help(help(usage),
                  " list [option ...] [file-or-directory ...]").
qlf_list:opt_help(recursive,
                  "Recurse into subdirectories").
qlf_list:opt_help(all,
                  "Also act on valid and up-to-date QLF files").

qlf_clean:opt_type(Flag, Opt, Type) :- qlf_list:opt_type(Flag, Opt, Type).
qlf_clean:opt_help(help(header),
                   [ansi(bold, "Delete out-of-date .qlf files.", [])]).
qlf_clean:opt_help(help(usage),
                   " clean [option ...] [file-or-directory ...]").
qlf_clean:opt_help(Opt, Message) :-
    qlf_list:opt_help(Opt, Message),
    atom(Opt).


%!  usage
%
%   Overall usage

qlf_command(compile, "Compile Prolog file to .qlf").
qlf_command(update,  "Recompile outdated .qlf files").
qlf_command(info,    "Print information on a .qlf file").
qlf_command(list,    "List .qlf files").
qlf_command(clean,   "Clean .qlf files").

usage :-
    argv_usage(debug).

opt_help(help(header),
         [ ansi(bold, 'Manage SWI-Prolog .qlf (Quick Load) files',  []),
           nl
         ]).
opt_help(help(usage),
         [ ' [option ...] '-[],
           ansi(bold, 'command', []),
           ' [arg ...]'-[]
         ]).
opt_help(help(footer),
         [ nl, ansi(comment, 'Available commands:', []), nl, nl,
           \commands,
           nl,
           'For help on a command use -h as command argument'
         ]).

commands -->
    foreach(qlf_command(Cmd, Comment),
            [ ansi(bold, '~w', [Cmd]),
              ansi(comment, '~t~15|~s~n', [Comment])
            ]).


		 /*******************************
		 *          SUB COMMANDS	*
		 *******************************/

cli_qlf_info(File, Options) :-
    option(source(true), Options),
    !,
    '$qlf_sources'(File, Sources),
    forall(member(F, Sources),
           writeln(F)).
cli_qlf_info(File, _Options) :-
    '$qlf_versions'(File, CurrentVersion, MinLOadVersion, FileVersion,
                    CurrentSignature, FileSignature),
    format('QLF version: ~p (current ~p, compatibility ~p)~n',
           [ FileVersion, CurrentVersion, MinLOadVersion ]),
    format('VM signature: 0x~16r (compatibility ox~16r)~n',
           [ FileSignature, CurrentSignature ]),
    (   catch('$qlf_is_compatible'(File), _, fail)
    ->  true
    ;   ansi_format(warning,
                    'QLF file is incompatible with this version of Prolog~n', [])
    ).

%!  cli_qlf_clean(+Files, +Options) is det.
%
%   Remove specified .qlf files.

cli_qlf_clean([], Options) :-
    option(recursive(true), Options),
    !,
    qlf_clean('.', Options).
cli_qlf_clean(Files, Options) :-
    forall(member(F, Files), qlf_clean(F, Options)).

qlf_clean(Dir, Options) :-
    exists_directory(Dir),
    option(recursive(true), Options),
    !,
    forall(directory_member(Dir, File,
                            [ recursive(true),
                              extensions([qlf])
                            ]),
           qlf_clean(File, Options)).
qlf_clean(File, _) :-
    \+ ( file_name_extension(_, Ext, File),
         user:prolog_file_type(Ext, qlf) ),
    !,
    ansi_format(warning, 'Ignoring ~w: not a QLF file~n', [File]).
qlf_clean(File, Options) :-
    catch('$qlf_is_compatible'(File), _, fail),
    !,
    (   option(all(true), Options)
    ->  print_message(informational, qlf(delete_file(File, all))),
        delete_file(File)
    ;   true
    ).
qlf_clean(File, _) :-
    print_message(informational, qlf(delete_file(File, incompatible))),
    delete_file(File),
    !.

%!  cli_qlf_update(+Files, +Options) is det.
%
%   Recompile .qlf files.

cli_qlf_update(Files, Options) :-
    forall(member(F, Files), qlf_update(F, Options)).

qlf_update(Dir, Options) :-
    exists_directory(Dir),
    option(recursive(true), Options),
    !,
    forall(directory_member(Dir, File,
                            [ recursive(true),
                              extensions([qlf])
                            ]),
           qlf_update(File, Options)).
qlf_update(File, _) :-
    \+ ( file_name_extension(_, Ext, File),
         user:prolog_file_type(Ext, qlf) ),
    !,
    ansi_format(warning, 'Ignoring ~w: not a QLF file~n', [File]).
qlf_update(File, Options) :-
    qlf_up_to_date(File),
    !,
    (   option(all(true), Options)
    ->  print_message(informational, qlf(recompile(File, all))),
        cli_qlf_compile(File, Options)
    ;   true
    ).
qlf_update(File, Options) :-
    print_message(informational, qlf(recompile(File, update))),
    cli_qlf_compile(File, Options).

qlf_up_to_date(File) :-
    '$qlf_versions'(File, CurrentVersion, _MinLOadVersion, FileVersion,
                    CurrentSignature, FileSignature),
    FileVersion == CurrentVersion,
    CurrentSignature == FileSignature,
    time_file(File, TQLF),
    '$qlf_sources'(File, Sources),
    E = error(_,_),
    forall(member(S, Sources),
          ( catch(time_file(S, TS), E, fail),
            TS < TQLF)).

%!  cli_qlf_list(+Files, +Options) is det.
%
%   List QLF files

cli_qlf_list(Files, Options) :-
    (   Files == [],
        option(recursive(true), Options)
    ->  qlf_list('.', Options)
    ;   forall(member(F, Files), qlf_list(F, Options))
    ).

qlf_list(Dir, Options) :-
    exists_directory(Dir),
    option(recursive(true), Options),
    !,
    forall(directory_member(Dir, File,
                            [ recursive(true),
                              extensions([qlf])
                            ]),
           qlf_list(File, Options)).
qlf_list(File, _) :-
    \+ ( file_name_extension(_, Ext, File),
         user:prolog_file_type(Ext, qlf) ),
    !,
    ansi_format(warning, 'Ignoring ~w: not a QLF file~n', [File]).
qlf_list(File, _Options) :-
    qlf_up_to_date(File),
    !,
    print_message(information, qlf(list(File, "up to date"))).
qlf_list(File, _Options) :-
    print_message(warning,     qlf(list(File, "needs to be rebuild"))).


		 /*******************************
		 *       compile SUPPORT	*
		 *******************************/

argv_deps_preload([], [], [], []).
argv_deps_preload(['--expect-deps'|Argv], Deps, Preload, Rest) :-
    !,
    argv_files(Argv, ArgvT, Deps, DepsT),
    argv_deps_preload(ArgvT, DepsT, Preload, Rest).
argv_deps_preload(['--preload'|Argv], Deps, Preload, Rest) :-
    !,
    argv_files(Argv, ArgvT, Preload, PreloadT),
    argv_deps_preload(ArgvT, Deps, PreloadT, Rest).
argv_deps_preload([H|T0], Deps, Preload, [H|Rest]) :-
    argv_deps_preload(T0, Deps, Preload, Rest).

argv_files([], [], Files, Files).
argv_files([Opt|Argv], [Opt|Argv], Files, Files) :-
    sub_atom(Opt, 0, _, _, '-'),
    !.
argv_files([File|Argv0], Argv, [File|Files0], Files) :-
    argv_files(Argv0, Argv, Files0, Files).

expand_compile_opt(include(true), include(user)) :- !.
expand_compile_opt(Opt, Opt).

cli_qlf_compile(Files, Options) :-
    is_list(Files),
    !,
    forall(member(File, Files),
           cli_qlf_compile(File, Options)).
cli_qlf_compile(File, Options) :-
    (   file_name_extension(Base, Ext, File),
        user:prolog_file_type(Ext, prolog)
    ->  true
    ;   Base = File
    ),
    option(preload(Preload), Options, []),
    forall(member(PeloadFile, Preload), preload(PeloadFile)),
    qcompile(user:Base, [imports([])|Options]),
    (   option(expect_deps(Deps), Options)
    ->  file_name_extension(Base, qlf, QlfFile),
        '$qlf_sources'(QlfFile, Sources),
        maplist(absolute_file_name, Deps, Canonical),
        subtract(Sources, Canonical, Missing),
        subtract(Canonical, Sources, Extra),
        (   Missing == []
        ->  true
        ;   print_message(warning, qcompile(missing, Base, Missing))
        ),
        (   Extra == []
        ->  true
        ;   print_message(warning, qcompile(extra, Base, Extra))
        )
    ;   true
    ).

preload(X) :-
    atom_concat('lib:', File, X),
    !,
    use_module(user:library(File)).
preload(X) :-
    use_module(user:X).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:message//1.

prolog:message(qcompile(missing, File, Dependencies)) -->
    [ 'The following dependencies for ~p are not listed'-[File], nl ],
    sequence(file, Dependencies, [nl]).
prolog:message(qcompile(extra, File, Dependencies)) -->
    [ 'The following dependencies for ~p are not needed'-[File], nl ],
    sequence(file, Dependencies, [nl]).
prolog:message(qlf(delete_file(File, Reason))) -->
    [ 'Deleting ~w (~w)'-[File, Reason] ].
prolog:message(qlf(list(File, Reason))) -->
    [ '~w (~w)'-[File, Reason] ].

file(File) -->
    [ '  ', url(File) ].

