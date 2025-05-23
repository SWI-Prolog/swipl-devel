/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1995-2016, University of Amsterdam
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

:- module('$qlf',
          [ qcompile/1,         % :Files
            qcompile/2,         % :Files, +Options
            '$qload_file'/5,    % +Path, +Module, -Ac, -LM, +Options
            '$qload_stream'/5   % +Stream, +Module, -Ac, -LM, +Options
          ]).


                 /*******************************
                 *         COMPILATION          *
                 *******************************/

:- meta_predicate
    qcompile(:),
    qcompile(:, +).

:- thread_local
    qinclude/1.

%!  qcompile(:Files) is det.
%
%   Compile Files as consult/1 and generate   a  Quick Load File for
%   each compiled file.

qcompile(M:Files) :-
    qcompile_(Files, M, []).
qcompile(M:Files, Options) :-
    '$option'(include(Incl), Options),
    !,
    '$must_be'(oneof(atom, include, [user]), Incl),
    setup_call_cleanup(
        asserta(qinclude(Incl), Ref),
        qcompile_(Files, M, Options),
        erase(Ref)).
qcompile(M:Files, Options) :-
    qcompile_(Files, M, Options).

qcompile_([], _, _) :- !.
qcompile_([H|T], M, Options) :-
    !,
    qcompile_(H, M, Options),
    qcompile_(T, M, Options).
qcompile_(FileName, Module, Options) :-
    absolute_file_name(FileName, Absolute,
                       [ file_type(source),
                         access(read),
                         file_errors(fail),
                         solutions(all)
                       ]),
    file_name_extension(ABase, PlExt, Absolute),
    \+ user:prolog_file_type(PlExt, qlf),
    !,
    once(user:prolog_file_type(QlfExt, qlf)),
    file_name_extension(ABase, QlfExt, Qlf),
    load_files(Module:Absolute, ['$qlf'(Qlf)|Options]).
qcompile_(FileName, _Module, _Options) :-
    absolute_file_name(FileName, Absolute,
                       [ file_type(prolog),
                         access(read)
                       ]),
    file_name_extension(_ABase, PlExt, Absolute),
    user:prolog_file_type(PlExt, qlf),
    throw(error(permission_error(compile, qlf, Absolute),
                context(qcompile/1, 'No Prolog source file'))).

%!  '$qload_file'(+File, +Module, -Action, -LoadedModule, +Options)
%
%   Load predicate for .qlf files.  See init.pl

'$qload_file'(File, Module, Action, LoadedModule, Options) :-
    setup_call_cleanup(
        qopen(File, In, Ref),
        setup_call_cleanup(
            '$save_lex_state'(LexState, Options),
            '$qload_stream'(In, Module,
                            Action, LoadedModule, Options),
            '$restore_lex_state'(LexState)),
        qclose(In, Ref)).

%!  qopen(+File, -Stream, -Ref) is det.
%
%   Open  a  .qlf  file  and   push    '$load_input'/2   to   make  sure
%   prolog_load_context(file, File) works correctly.

qopen(File, In, Ref) :-
    open(File, read, In, [type(binary)]),
    asserta(system:'$load_input'(File, In), Ref).

qclose(In, Ref) :-
    erase(Ref),
    close(In).

'$qload_stream'(In, Module, loaded, LoadedModule, Options) :-
    '$qlf_load'(Module:In, LM),
    check_is_module(LM, In, Options),
    (   atom(LM)
    ->  LoadedModule = LM
    ;   LoadedModule = Module
    ).

check_is_module(LM, In, Options) :-
    \+ atom(LM),
    '$option'(must_be_module(true), Options, false),
    !,
    stream_property(In, file_name(File)),
    throw(error(domain_error(module_file, File), _)).
check_is_module(_, _, _).
