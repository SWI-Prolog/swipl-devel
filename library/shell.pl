/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2014, University of Amsterdam
                              VU University Amsterdam
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

:- module(shell,
          [ shell/0,
            ls/0,
            ls/1,                               % +Pattern
            cd/0,
            cd/1,                               % +Dir
            pushd/0,
            pushd/1,                            % +Dir
            dirs/0,
            pwd/0,
            popd/0,
            mv/2,                               % +File1, +File2
            rm/1                                % +File1
          ]).
:- use_module(library(lists), [nth1/3]).
:- use_module(library(error)).
:- use_module(library(apply)).
:- set_prolog_flag(generate_debug_info, false).

/** <module>  Elementary shell commands

This library provides some basic  shell   commands  from Prolog, such as
=pwd=, =ls= for situations where there  is   no  shell  available or the
shell output cannot be captured.

It is developed on the ST-MINIX version.   MINIX  did not have a vfork()
call, and thus only allows shell/[0,1,2] if   Prolog uses less than half
the amount of available memory.
*/

%!  shell
%
%   Execute an interactive shell. The executed   shell is defined by
%   the environment =SHELL= or =comspec=   (Windows).  If neither is
%   defined, =|/bin/sh|= is used.

shell :-
    getenv('SHELL', Shell),        % Unix, also Cygwin
    !,
    shell(Shell).
shell :-
    getenv(comspec, ComSpec),      % Windows
    !,
    shell(ComSpec).
shell :-
    shell('/bin/sh').

%!  cd.
%!  cd(Dir).
%
%   Change working directory

cd :-
    cd(~).

cd(Dir) :-
    name_to_file(Dir, Name),
    working_directory(_, Name).

%!  pushd.
%!  pushd(+Dir).
%!  popd.
%!  dirs.
%
%   Manage the _directory stack_:
%
%     - pushd/1 is as cd/1, pushing th old directory on a stack
%     - pushd/0 swaps the current directory with the top of the
%       stack
%     - popd/0 pops to the top of the stack
%     - dirs/0 lists the current directory and the stack.

:- dynamic
    stack/1.

pushd :-
    pushd(+1).

pushd(N) :-
    integer(N),
    !,
    findall(D, stack(D), Ds),
    (   nth1(N, Ds, Go),
        retract(stack(Go))
    ->  pushd(Go),
        print_message(information, shell(directory(Go)))
    ;   warning('Directory stack not that deep', []),
        fail
    ).
pushd(Dir) :-
    name_to_file(Dir, Name),
    working_directory(Old, Name),
    asserta(stack(Old)).

popd :-
    retract(stack(Dir)),
    !,
    working_directory(_, Dir),
    print_message(information, shell(directory(Dir))).
popd :-
    warning('Directory stack empty', []),
    fail.

dirs :-
    working_directory(WD, WD),
    findall(D, stack(D), Dirs),
    maplist(dir_name, [WD|Dirs], Results),
    print_message(information, shell(file_set(Results))).

%!  pwd
%
%   Print current working directory

pwd :-
    working_directory(WD, WD),
    print_message(information, format('~w', [WD])).

dir_name('/', '/') :- !.
dir_name(Path, Name) :-
    atom_concat(P, /, Path),
    !,
    dir_name(P, Name).
dir_name(Path, Name) :-
    current_prolog_flag(unix, true),
    expand_file_name('~', [Home0]),
    (   atom_concat(Home, /, Home0)
    ->  true
    ;   Home = Home0
    ),
    atom_concat(Home, FromHome, Path),
    !,
    atom_concat('~', FromHome, Name).
dir_name(Path, Path).

%!  ls.
%!  ls(+Pattern).
%
%   Listing similar to Unix =ls -F=, flagging directories with =/=.

ls :-
    ls('.').

ls(Spec) :-
    name_to_files(Spec, Matches),
    ls_(Matches).

ls_([]) :-
    !,
    warning('No Match', []).
ls_([Dir]) :-
    exists_directory(Dir),
    !,
    atom_concat(Dir, '/*', Pattern),
    expand_file_name(Pattern, Files),
    maplist(tagged_file_in_dir, Files, Results),
    print_message(information, shell(file_set(Results))).
ls_(Files) :-
    maplist(tag_file, Files, Results),
    print_message(information, shell(file_set(Results))).

tagged_file_in_dir(File, Result) :-
    file_base_name(File, Base),
    (   exists_directory(File)
    ->  atom_concat(Base, /, Result)
    ;   Result = Base
    ).

tag_file(File, Dir) :-
    exists_directory(File),
    !,
    atom_concat(File, /, Dir).
tag_file(File, File).

%!  mv(+From, +To) is det.
%
%   Move (Rename) a file. If To is   a directory, From is moved into
%   the directory.

mv(From, To) :-
    name_to_files(From, Src),
    name_to_file(To, Dest),
    mv_(Src, Dest).

mv_([One], Dest) :-
    \+ exists_directory(Dest),
    !,
    rename_file(One, Dest).
mv_(Multi, Dest) :-
    (   exists_directory(Dest)
    ->  maplist(mv_to_dir(Dest), Multi)
    ;   print_message(warning, format('Not a directory: ~w', [Dest])),
        fail
    ).

mv_to_dir(Dest, Src) :-
    file_base_name(Src, Name),
    atomic_list_concat([Dest, Name], /, Target),
    rename_file(Src, Target).

%!  rm(+File) is det.
%
%   Remove (unlink) a file

rm(File) :-
    name_to_file(File, A),
    delete_file(A).


%!  name_to_file(+Name, -File)
%
%   Convert Name into a single file.

name_to_file(Spec, File) :-
    name_to_files(Spec, Files),
    (   Files = [File]
    ->  true
    ;   print_message(warning, format('Ambiguous: ~w', [Spec])),
        fail
    ).

name_to_files(Spec, Files) :-
    name_to_files_(Spec, Files),
    (   Files == []
    ->  print_message(warning, format('No match: ~w', [Spec])),
        fail
    ;   true
    ).

name_to_files_(Spec, Files) :-
    compound(Spec),
    compound_name_arity(Spec, _Alias, 1),
    !,
    findall(File,
            (   absolute_file_name(Spec, File,
                                   [ access(exist),
                                     file_type(directory),
                                     file_errors(fail),
                                     solutions(all)
                                   ])
            ;   absolute_file_name(Spec, File,
                                   [ access(exist),
                                     file_errors(fail),
                                     solutions(all)
                                   ])
            ),
            Files).
name_to_files_(Spec, Files) :-
    (   atomic(Spec)
    ->  S1 = Spec
    ;   phrase(segments(Spec), L),
        atomic_list_concat(L, /, S1)
    ),
    expand_file_name(S1, Files0),
    (   Files0 == [S1],
        \+ access_file(S1, exist)
    ->  warning('"~w" does not exist', [S1]),
        fail
    ;   Files = Files0
    ).

segments(Var) -->
    { var(Var),
      !,
      instantiation_error(Var)
    }.
segments(A/B) -->
    !,
    segments(A),
    segments(B).
segments(A) -->
    { must_be(atomic, A) },
    [ A ].

%!  warning(+Fmt, +Args:list) is det.

warning(Fmt, Args) :-
    print_message(warning, format(Fmt, Args)).

:- multifile prolog:message//1.

prolog:message(shell(file_set(Files))) -->
    { catch(tty_size(_, Width), _, Width = 80)
    },
    table(Files, Width).
prolog:message(shell(directory(Path))) -->
    { dir_name(Path, Name) },
    [ '~w'-[Name] ].

%!  table(+List, +Width)//
%
%   Produce a tabular layout to list all   elements of List on lines
%   with a maximum width of Width. Elements are placed as =ls= does:
%
%      ==
%      1  4  7
%      2  5  8
%      3  6
%      ==

table(List, Width) -->
    { table_layout(List, Width, Layout),
      compound_name_arguments(Array, a, List)
    },
    table(0, Array, Layout).

table(I, Array, Layout) -->
    { Cols = Layout.cols,
      Index is I // Cols + (I mod Cols) * Layout.rows + 1,
      (   (I+1) mod Cols =:= 0
      ->  NL = true
      ;   NL = false
      )
    },
    (   { arg(Index, Array, Atom) }
    ->  (   { NL == false }
        ->  [ '~|~w~t~*+'-[Atom, Layout.col_width] ]
        ;   [ '~w'-[Atom] ]
        )
    ;   []
    ),
    (   { I2 is I+1,
          I2 < Cols*Layout.rows
        }
    ->  (   { NL == true }
        ->  [ nl ]
        ;   []
        ),
        table(I2, Array, Layout)
    ;   []
    ).

table_layout(Atoms, Width, _{cols:Cols, rows:Rows, col_width:ColWidth}) :-
    length(Atoms, L),
    longest(Atoms, Longest),
    Cols is max(1, Width // (Longest + 3)),
    Rows is integer(L / Cols + 0.49999),    % should be ceil/1
    ColWidth is Width // Cols.

longest(List, Longest) :-
    longest(List, 0, Longest).

longest([], M, M) :- !.
longest([H|T], Sofar, M) :-
    atom_length(H, L),
    L >= Sofar,
    !,
    longest(T, L, M).
longest([_|T], S, M) :-
    longest(T, S, M).

