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

:- module(app_qlf, []).

:- use_module(library(main)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(dcg/high_order)).
:- use_module(library(option)).
:- use_module(library(strings)).
:- use_module(library(ansi_term)).

:- initialization(main, main).

main(Argv) :-
    argv_deps_preload(Argv, Deps, Preload, Argv1),
    (   memberchk('--expect-deps', Argv)
    ->  Options = [expect_deps(Deps)|Options1]
    ;   Options = Options1
    ),
    (   memberchk('--preload', Argv)
    ->  Options1 = [preload(Preload)|Options2]
    ;   Options1 = Options2
    ),
    argv_options(Argv1, Pos, Options2),
    run(Pos, Options).
main(_) :-
    argv_usage(debug).

		 /*******************************
		 *             USAGE		*
		 *******************************/

opt_type(compile,   compile,   boolean).
opt_type(c,         compile,   boolean).
opt_type(source,    source,    boolean).
opt_type(s,         source,    boolean).
opt_type(version,   version,   boolean).
opt_type(v,         version,   boolean).
opt_type(clean,     clean,     boolean).
opt_type(update,    update,    boolean).
opt_type(list,      list,      boolean).
opt_type(l,         list,      boolean).
opt_type(recursive, recursive, boolean).
opt_type(r,         recursive, boolean).
opt_type(all,       all,       boolean).
opt_type(a,         all,       boolean).

opt_help(compile,
         "Compile Prolog files").
opt_help(source,
         "List the source files from which this QLF file was created").
opt_help(version,
         "List version information about QLF file").
opt_help(clean,
         "Remove all QLF files that are incompatible with this version of Prolog").
opt_help(update,
         "Recompile all QLF files that are incompatible or out-of-date").
opt_help(list,
         "List QLF files").
opt_help(recursive,
         "Recurse into subdirectories").
opt_help(all,
         "Also act on valid and up-to-date QLF files").

opt_help(help(header),
         md({|string||
# SWI-Prolog QLF (Quick Load Files) utility

The __qlf__ tool provides commandline friendly interaction with QLF
files.  It is primarily intended to support build tools.
            |})).

opt_help(help(usage),
         " [--compile] file[.pl] [--expect-deps file.pl ...] [--preload file.pl ...]").
opt_help(help(usage),
         " --source file.qlf").
opt_help(help(usage),
         " --version file.qlf").
opt_help(help(usage),
         " --list [--recursive] [--all] file.qlf|dir ...").
opt_help(help(usage),
         " --clean [--recursive] [--all] file.qlf|dir ...").
opt_help(help(usage),
         " --update [--recursive] [--all] file.qlf|dir ...").
opt_help(help(description),
         md({|string||
## Command descriptions

  - ``--compile`` [option..] file.pl<br>
    Compile a Prolog source to QLF.  The ``--preload`` option first
    loads possible requirements such as expansion rules.  The
    ``-expect-deps`` option is intended for build tools.  It allows
    the build tool to specify the files it has registered as
    inputs.  If this does not match the actual inputs a warning is
    printed.  The inputs for a qlf file can be found using ``--sources``.

  - ``--source`` file.qlf<br>
    Print the source file that are included into the QLF file.

  - ``--version`` file.qlf<br>
    Print compatibility version info on the QLF file

  - ``--list``<br>
    List QLF files and whether or not they are up-to-date.  For
    example, list all QLF files in the current directory and its
    sub directories:

    ```
    swipl qlf -lr .
    ```
  - `--clean`<br>
    Clean QLF files.  By default cleans incompatible or out-of-date
    QLF files.  Using ``--all``, all QLF files are removed.  For
    example, to remove all QLF files in current directory and its
    sub directories:

    ```
    swipl qlf --clean -ra .
    ```
  - ``--update``<br>
    Update any out-of-date or incompatible QLF file.  Processes
    the ``--reload file ..` option.

            |})).

		 /*******************************
		 *           MISC SUPPORT	*
		 *******************************/

run([File], Options) :-
    option(compile(true), Options),
    !,
    qlf_compile(File, Options).
run([File], Options) :-
    option(source(true), Options),
    !,
    '$qlf_sources'(File, Sources),
    forall(member(F, Sources),
           writeln(F)).
run([File], Options) :-
    option(version(true), Options),
    !,
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
run(Files, Options) :-
    option(clean(true), Options),
    !,
    forall(member(F, Files), qlf_clean(F, Options)).
run(Files, Options) :-
    option(update(true), Options),
    !,
    forall(member(F, Files), qlf_update(F, Options)).
run(Files, Options) :-
    option(list(true), Options),
    !,
    (   Files == [],
        option(recursive(true), Options)
    ->  qlf_list('.', Options)
    ;   forall(member(F, Files), qlf_list(F, Options))
    ).
run([File], Options) :-
    file_name_extension(_, Ext, File),
    user:prolog_file_type(Ext, prolog),
    !,
    run([File], [compile(true)|Options]).


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
        qlf_compile(File, Options)
    ;   true
    ).
qlf_update(File, Options) :-
    print_message(informational, qlf(recompile(File, update))),
    qlf_compile(File, Options).

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
		 *       --build support	*
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

qlf_compile(File, Options) :-
    (   file_name_extension(Base, Ext, File),
        user:prolog_file_type(Ext, prolog)
    ->  true
    ;   Base = File
    ),
    option(preload(Preload), Options, []),
    forall(member(PeloadFile, Preload), preload(PeloadFile)),
    qcompile(user:Base),
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

