/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(app_pack, []).

:- use_module(library(prolog_pack)).
:- use_module(library(main)).
:- use_module(library(dcg/high_order)).
:- use_module(library(apply)).
:- use_module(library(strings)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

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
pack(search, Argv) =>
    pack_find:argv_options(Argv, Pos, Options),
    cli_pack_find(Pos, Options).
pack(info, Argv) =>
    pack_info:argv_options(Argv, Pos, Options),
    cli_pack_info(Pos, Options).
pack(install, Argv) =>
    pack_install:argv_options(Argv, Pos, Options),
    cli_pack_install(Pos, Options).
pack(rebuild, Argv) =>
    pack_rebuild:argv_options(Argv, Pos, Options),
    cli_pack_rebuild(Pos, Options).
pack(remove, Argv) =>
    pack_remove:argv_options(Argv, Pos, Options),
    cli_pack_remove(Pos, Options).
pack(publish, Argv) =>
    pack_publish:argv_options(Argv, Pos, Options),
    cli_pack_publish(Pos, Options).
pack(help, [Command]) =>
    pack_command(Command, _),
    atom_concat(pack_, Command, Module),
    argv_usage(Module:debug).
pack(_, _) =>
    argv_usage(debug).

pack_command(list,    "List packs").
pack_command(find,    "Find packs").
pack_command(search,  "Alias for `find`").
pack_command(info,    "Print info on a pack").
pack_command(install, "Install or upgrade a pack").
pack_command(rebuild, "Recompile foreign parts for a pack").
pack_command(remove,  "Uninstall a pack").
pack_command(publish, "Register a pack with swi-prolog.org").
pack_command(help,    "Help on command (also swipl pack command -h)").

pack_find:opt_type(_,_,_) :- fail.

pack_info:opt_type(dir, pack_directory, directory).
pack_info:opt_help(pack_directory, "Pack directory").

pack_remove:opt_type(y,    interactive,    boolean(false)).
pack_remove:opt_type(deps, dependencies,   boolean).
pack_remove:opt_type(dir,  pack_directory, directory).

pack_remove:opt_help(interactive,    "Use default answers (non-interactive)").
pack_remove:opt_help(dependencies,   "Remove dependencies as well?").
pack_remove:opt_help(pack_directory, "Remove pack below directory").

pack_list:opt_type(installed, installed,      boolean).
pack_list:opt_type(i,         installed,      boolean).
pack_list:opt_type(outdated,  outdated,       boolean).
pack_list:opt_type(server,    server,         (boolean|atom)).
pack_list:opt_type(dir,       pack_directory, directory).

pack_list:opt_meta(server, 'URL|false').

pack_list:opt_help(installed,      "Only list installed packages").
pack_list:opt_help(outdated,       "Only list packages that can be upgraded").
pack_list:opt_help(server,         "Use as `--no-server` or `server=URL`").
pack_list:opt_help(pack_directory, "Directory for --installed").

pack_install:opt_type(url,      url,            atom).
pack_install:opt_type(dir,      pack_directory, directory(write)).
pack_install:opt_type(autoload, autoload,	boolean).
pack_install:opt_type(global,   global,         boolean).
pack_install:opt_type(y,        interactive,    boolean(false)).
pack_install:opt_type(quiet,    silent,         boolean).
pack_install:opt_type(q,        silent,         boolean).
pack_install:opt_type(upgrade,  upgrade,        boolean).
pack_install:opt_type(u,        upgrade,        boolean).
pack_install:opt_type(insecure, insecure,       boolean).
pack_install:opt_type(k,        insecure,       boolean).
pack_install:opt_type(rebuild,  rebuild,        (boolean|
                                                 oneof([if_absent,make]))).
pack_install:opt_type(test,     test,           boolean).
pack_install:opt_type(git,      git,            boolean).
pack_install:opt_type(link,     link,           boolean).
pack_install:opt_type(version,  version,        atom).
pack_install:opt_type(branch,   branch,         atom).
pack_install:opt_type(commit,   commit,         atom).
pack_install:opt_type(server,   server,         atom).

pack_install:opt_help(url,            "Explicit GIT or download location").
pack_install:opt_help(pack_directory, "Install in DIR/<pack>").
pack_install:opt_help(global,         "Install system-wide (default: user)").
pack_install:opt_help(interactive,    "Use default answers (non-interactive)").
pack_install:opt_help(silent,         "Do not print informational feedback").
pack_install:opt_help(autoload,       "Make the library available for \c
                                       autoloading").
pack_install:opt_help(upgrade,        "Upgrade the package").
pack_install:opt_help(insecure,       "Do not check TLS certificates").
pack_install:opt_help(rebuild,        "Rebuilt foreign components").
pack_install:opt_help(test,           "Run test suite (if any)").
pack_install:opt_help(git,            "Interpret URL as a GIT repository").
pack_install:opt_help(link,           "Install from local directory using \c
                                       a symbolic link").
pack_install:opt_help(version,        "Restrict the version.").
pack_install:opt_help(branch,         "Checkout GIT branch.").
pack_install:opt_help(commit,         "Checkout GIT commit.").
pack_install:opt_help(server,         "Server to contact for finding packages. \c                                       Default is https://www.swi-prolog.org.").

pack_install:opt_help(help(usage),
                      " install [option ...] pack ...").
pack_install:opt_help(help(footer),
                      [ nl,
                        ansi(bold, 'Examples:', []), nl, nl,
                        ansi(code, '  swipl pack install ', []), '<pack>'
                      ]).

pack_install:opt_meta(version,	   '[CMP]VERSION').
pack_install:opt_meta(rebuild,	   'WHEN').
pack_install:opt_meta(url,	   'URL').
pack_install:opt_meta(branch,	   'BRANCH').
pack_install:opt_meta(commit,	   'HASH').
pack_install:opt_meta(server,	   'URL').

pack_rebuild:opt_type(dir, pack_directory, directory).

pack_rebuild:opt_help(help(usage),
                      " rebuild [--dir=DIR] [pack ...]").
pack_rebuild:opt_help(pack_directory, "Rebuild packs in directory").

pack_publish:opt_type(git,      git,            boolean).
pack_publish:opt_type(sign,     sign,           boolean).
pack_publish:opt_type(force,    force,          boolean).
pack_publish:opt_type(branch,   branch,         atom).
pack_publish:opt_type(register, register,       boolean).
pack_publish:opt_type(isolated, isolated,       boolean).
pack_publish:opt_type(dir,      pack_directory, directory(write)).
pack_publish:opt_type(clean,    clean,          boolean(write)).
pack_publish:opt_type(server,   server,         atom).

pack_publish:opt_help(register,       "Register at pack repository").
pack_publish:opt_help(isolated,       "Isolate from my other packs").
pack_publish:opt_help(pack_directory, "Build directory").
pack_publish:opt_help(clean,          "Clean build directory first").
pack_publish:opt_help(git,            "Publish from GIT repository").
pack_publish:opt_help(sign,           "Sign the git release tag").
pack_publish:opt_help(force,          "Force (update) the git release tag").
pack_publish:opt_help(branch,         "Branch used for releases").
pack_publish:opt_help(server,         "Server to publish package on. \c                                                 Default is https://www.swi-prolog.org.").
pack_publish:opt_help(help(usage),
                      " publish [option ...] url|dir").
pack_publish:opt_help(
    help(header),
    md({|string||
        | # Publish a SWI-Prolog pack
        |
        |})).
pack_publish:opt_help(
    help(footer),
    md({|string||
        | Once your pack is completed, it may be registered at
        | ``https://www.swi-prolog.org/pack/list``.  The __publish__
        | sub command of ``swipl pack`` installs your pack from the
        | given location, notmally in an isolated temporary directory.
        | After successful installation it informs the pack registry
        | of the new pack and deletes the temporary directory.
        |
        | # Examples:
        |
        | The typical command to publish a pack from a git repository is
        |
        |     swipl pack publish .
        |
        | The above requires the _origin_ of the repo to point at a publically
        | accessible git repository.
        |
        | If your pack is hosted as archive, the typical command is
        |
        |     swipl pack publish https://mydomain.org/mydownloads/mypack-1.2.3.zip
        |})).

pack_publish:opt_meta(branch, 'BRANCH').
pack_publish:opt_meta(server, 'URL').

cli_pack_list(Pos, Options),
    select_option(pack_directory(Dir), Options, Options1) =>
    attach_packs(Dir, [replace(true)]),
    cli_pack_list(Pos, [installed(true)|Options1]).
cli_pack_list([], Options) =>
    pack_list('', [installed(true)|Options]).
cli_pack_list([Search], Options) =>
    pack_list(Search, [installed(true)|Options]).
cli_pack_list(_, _) =>
    argv_usage(pack_list:debug).

cli_pack_find([], Options) =>
    pack_list('', Options).
cli_pack_find([Search], Options) =>
    pack_list(Search, Options).
cli_pack_find(_, _) =>
    argv_usage(pack_find:debug).

cli_pack_info(Pos, Options),
    select_option(pack_directory(Dir), Options, Options1) =>
    attach_packs(Dir, [replace(true)]),
    cli_pack_info(Pos, Options1).
cli_pack_info([Pack], _) =>
    cli(pack_info(Pack)).
cli_pack_info(_, _) =>
    argv_usage(pack_info:debug).

cli_pack_install(Packs, Options), Packs \== [] =>
    maplist(parse_option, Options, Options1),
    cli(pack_install(Packs, Options1)).
cli_pack_install(_, _) =>
    argv_usage(pack_install:debug).

cli_pack_rebuild(Packs, Options),
    select_option(pack_directory(Dir), Options, Options1) =>
    attach_packs(Dir, [replace(true)]),
    cli_pack_rebuild(Packs, Options1).
cli_pack_rebuild([], _Options) =>
    cli(pack_rebuild).
cli_pack_rebuild(Packs, _Options) =>
    cli(forall(member(Pack, Packs), pack_rebuild(Pack))).

cli_pack_remove(Packs, Options),
    select_option(pack_directory(Dir), Options, Options1) =>
    attach_packs(Dir, [replace(true)]),
    cli_pack_remove(Packs, Options1).
cli_pack_remove(Packs, Options), Packs \== [] =>
    cli(forall(member(Pack, Packs), pack_remove(Pack, Options))).
cli_pack_remove(_, _) =>
    argv_usage(pack_remove:debug).


cli_pack_publish([URL], Options) :-
    cli(pack_publish(URL, Options)).

:- meta_predicate
    cli(0).

cli(Command) :-
    catch(Command, E, print_message(error, E)), !.
cli(_Command) :-
    format('~N'),
    halt(1).

parse_option(version(Atom), version(Version)) :-
    atom_codes(Atom, Codes),
    phrase(version(Version), Codes),
    !.
parse_option(Opt, Opt).

version(Version) -->
    cmp(Cmp), !, whites,
    remainder(String),
    !,
    { atom_codes(V, String),
      Version =.. [Cmp,V]
    }.

cmp(>=) --> ">=".
cmp(>)  --> ">".
cmp(=<) --> "=<".
cmp(=<) --> "<=".
cmp(<)  --> "<".
cmp(=)  --> "=".
cmp(=)  --> "==".

usage :-
    argv_usage(debug).

opt_help(help(header),
         md("__Manage SWI-Prolog packs__

            SWI-Prolog packs are community contributed libraries
            and applications.  You can find the available packs at

                https://www.swi-prolog.org/pack/list

            ")).
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
