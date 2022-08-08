/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

:- module(wasm,
          [ wasm_query_loop/0,
            wasm_abort/0
          ]).

/** <module> WASM version support
*/

%!  wasm_query_loop

wasm_query_loop :-
    current_prolog_flag(heartbeat, Old),
    setup_call_cleanup(
        set_prolog_flag(heartbeat, 1000),
        '$toplevel':'$query_loop',
        set_prolog_flag(heartbeat, Old)).

%!  wasm_abort
%
%   Execution aborted by user

wasm_abort :-
    print_message(error, '$aborted'),
    abort.

:- multifile
    prolog:heartbeat/0.

%!  prolog:heartbeat
%
%   Called after setting the Prolog  flag   `heartbeat`  to non-zero. If
%   possible, we yield control back to JavaScript

prolog:heartbeat :-
    (   '$can_yield'
    ->  js_yield(beat, Reply),
        (   Reply == "true"
        ->  true
        ;   term_string(Goal, Reply),
            ignore(call(Goal))
        )
    ;   true
    ).
