/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions b.v.
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

:- module(wasm_shell, []).
:- use_module(library(wasm)).
:- use_module(library(ansi_term)).

:- multifile prolog_edit:edit_source/1.

%!  prolog_edit:edit_source(++Spec)
%
%   Make edit/1 work by filling the editor and trying to select the
%   right line.

prolog_edit:edit_source(Spec) :-
    memberchk(file(File), Spec),
    load_file(File, String),
    _ := addFileOption(#File),
    _ := switchToFile(#File),
    _ := cm.setValue(String),
    (   memberchk(line(Line), Spec)
    ->  _ := cm.scrollIntoView(_{line:Line, ch:0}, 200)
    ;   true
    ).

load_file(Spec, String) :-
    uri_is_global(Spec),
    !,
    fetch(Spec, text, String).
load_file(Spec, String) :-
    setup_call_cleanup(
        open(Spec, read, In),
        read_string(In, _Len, String),
        close(In)).

%!  trace_action(+Action, +Message) is det.
%
%   Perform actions on behalf of the debugger, such as printing the
%   current goal, etc.
%
%   @arg Message is a term frame(Frame, Choice, Port, PC) as provided
%   by PL_get_trace_context()

trace_action(print, Msg) =>
    print_message(debug, Msg).
trace_action(goals, frame(Frame,_Choice,_Port,_PC)) =>
    dbg_backtrace(Frame, 5).

dbg_backtrace(Frame, Depth) :-
    get_prolog_backtrace(Depth, Stack,
                         [ frame(Frame),
                           goal_term_depth(10)
                         ]),
    print_prolog_backtrace(user_error, Stack,
                           [ show_files(basename)
                           ]).
