/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
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

:- use_module(library(prolog_pack)).
:- use_module(library(main)).
:- use_module(library(dcg/high_order)).

:- initialization(main, main).

main([Argv1|Argv]) :-
    pack(Argv1, Argv),
    !.
main(_) :-
    usage,
    halt(1).

pack(list, Argv) =>
    pack_list:argv_options(Argv, Pos, Options),
    cli_pack_list(Pos, Options).
pack(find, Argv) =>
    pack_find:argv_options(Argv, Pos, Options),
    cli_pack_find(Pos, Options).
pack(info, Argv) =>
    pack_info:argv_options(Argv, Pos, Options),
    cli_pack_info(Pos, Options).
pack(install, Argv) =>
    pack_install:argv_options(Argv, Pos, Options),
    cli_pack_install(Pos, Options).
pack(remove, Argv) =>
    pack_remove:argv_options(Argv, Pos, Options),
    cli_pack_remove(Pos, Options).
pack(help, [Command]) =>
    pack_command(Command, _),
    atom_concat(pack_, Command, Module),
    argv_usage(Module:debug).
pack(_, _) =>
    argv_usage(debug).

pack_command(list,    "List packages").
pack_command(find,    "Find packages").
pack_command(install, "Install a package").
pack_command(remove,  "Uninstall a package").
pack_command(help,    "Help on command (also swipl pack command -h)").

pack_find:opt_type(_,_,_) :- fail.
pack_info:opt_type(_,_,_) :- fail.
pack_remove:opt_type(_,_,_) :- fail.

pack_list:opt_type(outdated, outdated, boolean).
pack_list:opt_help(outdated, "Only list packages that can be upgraded").

pack_install:opt_type(url,         url,               atom).
pack_install:opt_type(dir,         package_directory, directory(write)).
pack_install:opt_type(global,      global,            boolean).
pack_install:opt_type(y,	   interactive,       boolean(false)).
pack_install:opt_type(quiet,       silent,            boolean).
pack_install:opt_type(q,           silent,            boolean).
pack_install:opt_type(upgrade,     upgrade,           boolean).
pack_install:opt_type(u,           upgrade,           boolean).
pack_install:opt_type(insecure,    insecure,          boolean).
pack_install:opt_type(k,	   insecure,          boolean).
pack_install:opt_type(rebuild,     rebuild,           (boolean|
                                                       oneof([if_absent,make]))).
pack_install:opt_type(test,        test,              boolean).
pack_install:opt_type(git,         git,               boolean).
pack_install:opt_type(link,        link,              boolean).

pack_install:opt_help(url,               "Explicit GIT or download location").
pack_install:opt_help(package_directory, "Install in DIR/<pack>").
pack_install:opt_help(global,            "Install system-wide (default: user)").
pack_install:opt_help(interactive,       "Use default answers (non-interactive)").
pack_install:opt_help(silent,            "Do not print informational feedback").
pack_install:opt_help(upgrade,           "Upgrade the package").
pack_install:opt_help(insecure,          "Do not check TLS certificates").
pack_install:opt_help(rebuild,           "Rebuilt foreign components").
pack_install:opt_help(test,              "Run test suite (if any)").
pack_install:opt_help(git,               "Interpret URL as a GIT repository").
pack_install:opt_help(link,              "Install from local directory using \c
					  a symbolic link").

pack_install:opt_help(help(footer),
                      [ nl,
                        ansi(bold, 'Examples:', []), nl, nl,
                        ansi(code, '  swipl pack install ', []), '<pack>'
                      ]).

pack_install:opt_meta(rebuild,	   'WHEN').
pack_install:opt_meta(url,	   'URL').

cli_pack_list([], Options) =>
    pack_list('', [installed(true)|Options]).
cli_pack_list([Search], Options) =>
    pack_list(Search, [installed(true)|Options]).
cli_pack_list(_, _) =>
    argv_usage(pack_list:debug).

cli_pack_find([], _Options) =>
    pack_list('').
cli_pack_find([Search], _Options) =>
    pack_list(Search).
cli_pack_find(_, _) =>
    argv_usage(pack_find:debug).

cli_pack_info([Pack], _) =>
    cli(pack_info(Pack)).
cli_pack_info(_, _) =>
    argv_usage(pack_info:debug).

cli_pack_install([Pack], []) =>
    cli(pack_install(Pack)).
cli_pack_install([Pack], Options) =>
    cli(pack_install(Pack, Options)).
cli_pack_install(_, _) =>
    argv_usage(pack_install:debug).

cli_pack_remove([Pack], _) =>
    cli(pack_remove(Pack)).
cli_pack_remove(_, _) =>
    argv_usage(pack_remove:debug).


:- meta_predicate
    cli(0).

cli(Command) :-
    catch(Command, E, print_message(error, E)), !.
cli(_Command) :-
    format('~N'),
    halt(1).

usage :-
    argv_usage(debug).

opt_help(help(header),
         [ ansi(bold, 'Manage SWI-Prolog packages',  []),
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
    foreach(pack_command(Cmd, Comment),
            [ ansi(bold, '~w', [Cmd]),
              ansi(comment, '~t~15|~s~n', [Comment])
            ]).
