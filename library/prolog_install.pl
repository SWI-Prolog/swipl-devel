/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2018, VU University Amsterdam
                              CWI, Amsterdam
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

:- module(prolog_install,
          [ qcompile_libraries/0,
            cmake_qcompile/0,
            cmake_qcompile/2,                   % +File, +Deps
            cmake_save_man_index/0
          ]).
:- use_module(library(make)).
:- use_module(library(apply)).
:- use_module(library(lists)).

/** <module> Installation support predicates

This module provides helper predicates for build and install steps.
*/

%!  qcompile_libraries
%
%   Quick-load compilation of the Prolog libraries.

qcompile_libraries :-
    make,                           % update library index
    qcompile_xpce.

qcompile_xpce :-                        % no XPCE around
    \+ absolute_file_name(swi(xpce),
                          [ access(exist),
                            file_type(directory),
                            file_errors(fail)
                          ], _),
    !,
    print_message(informational, qcompile(no(xpce))).
qcompile_xpce :-
    (   absolute_file_name(swi('swipl-win.rc'), _,
                           [ access(read),
                             file_errors(fail)
                           ])
    ->  use_module(swi('swipl-win.rc'))
    ;   true
    ),
    qcompile_libs.


                 /*******************************
                 *       PRECOMPILED PARTS      *
                 *******************************/

qmodule(pce, library(pce)).
qmodule(lib, library(pce_manual)).
qmodule(lib, library(pcedraw)).
qmodule(lib, library('emacs/emacs')).
qmodule(lib, library('dialog/dialog')).
qmodule(lib, library('trace/trace')).
qmodule(lib, library('cql/cql')).

qcompile_libs :-
    forall(qmodule(_Type, Module),
           (   exists_source(Module)
           ->  print_message(informational, qcompile(Module)),
               qcompile(Module)
           ;   print_message(informational, qcompile(no(Module)))
           )).

		 /*******************************
		 *   QLF COMPILATION FOR BUILD	*
		 *******************************/

%!  cmake_qcompile(+File, +Deps) is det.
%
%   Qcompile on behalf of CMAKE installation.  File   is  the file to be
%   qcompiled. Deps is the dependencies as   CMAKE  believes to be true.
%   Reports on inconsistent dependencies

cmake_qcompile :-
    current_prolog_flag(argv, ['--compile', File, '--qlfdeps' | Rest]),
    (   append(Deps, ['--preload'|Preload], Rest)
    ->  true
    ;   Deps = Rest,
        Preload = []
    ),
    forall(member(X, Preload), preload(X)),
    cmake_qcompile(File, Deps).

preload(X) :-
    atom_concat('lib:', File, X),
    !,
    use_module(user:library(File)).
preload(X) :-
    use_module(user:X).


cmake_qcompile(File, Deps) :-
    qcompile(File),
    file_name_extension(File, qlf, QlfFile),
    '$qlf_info'(QlfFile,
                _CurrentVersion, _MinLOadVersion, _FileVersion,
                _CurrentSignature, _FileSignature,
                _WordSize,
                Files),
    maplist(absolute_file_name, Deps, Canonical),
    subtract(Files, Canonical, Missing),
    subtract(Canonical, Files, Extra),
    (   Missing == []
    ->  true
    ;   print_message(warning, qcompile(missing, File, Missing))
    ),
    (   Extra == []
    ->  true
    ;   print_message(warning, qcompile(extra, File, Extra))
    ).

		 /*******************************
		 *        MANUAL SUPPORT	*
		 *******************************/

%!  cmake_save_man_index
%
%   Create swi('doc/manindex.db') during the build process.

cmake_save_man_index :-
    use_module(library(pldoc/man_index)),
    save_man_index.


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:message//1.

prolog:message(qcompile(no(What))) -->
    [ 'Cannot find ~w'-[What] ].
prolog:message(qcompile(library(Lib))) -->
    [ nl, '~*c'-[64, 0'*], nl ],
    [ 'Qcompile library ~q'-[Lib], nl ],
    [ '~*c'-[64, 0'*] ].
prolog:message(qcompile(missing, File, Dependencies)) -->
    [ 'The following dependencies for ~p are not listed'-[File] ],
    deps(Dependencies).
prolog:message(qcompile(extra, File, Dependencies)) -->
    [ 'The following dependencies for ~p are not needed'-[File] ],
    deps(Dependencies).

deps([]) -->
    [].
deps([H|T]) -->
    [ nl, '  ~p'-[H] ],
    deps(T).
