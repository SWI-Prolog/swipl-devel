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
            wasm_abort/0,
            wasm_call_string/3,         % +String, +Input, -Output
            sleep/1
          ]).
:- autoload(library(apply), [exclude/3]).

/** <module> WASM version support
*/

:- meta_predicate wasm_call_string(:, +, -).

%!  wasm_query_loop

wasm_query_loop :-
    current_prolog_flag(heartbeat, Old),
    setup_call_cleanup(
        set_prolog_flag(heartbeat, 10 000),
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

%!  wasm_call_string(+Goal:string, +Input, -Result) is nondet.
%
%   Run a Prolog goal from  a  string,   returning  a  dict  holding the
%   variable bindings in Result. Variables   starting with an underscore
%   are ignored.   This allows for
%
%   ```
%     for(const answer on Prolog.query("p(X)")) {
%       console.log(answer.X);
%     }
%   ```

wasm_call_string(M:String, Input, Dict) :-
    term_string(Goal, String, [variable_names(Map)]),
    exclude(not_in_projection(Input), Map, Map1),
    dict_create(Dict, bindings, Map1),

    call(M:Goal).

not_in_projection(Input, Name=Value) :-
    (   get_dict(Name, Input, Value)
    ->  true
    ;   sub_atom(Name, 0, _, _, '_')
    ).


%!  sleep(+Seconds)
%
%   Sleep by yielding when possible. Note   that this defines sleep/1 in
%   `user`, overruling system:sleep/1.

sleep(Seconds) :-
    (   '$can_yield'
    ->  format(string(Command), '{"command":"sleep","time":~w}', [Seconds]),
        js_yield(Command, Reply),
        term_string(Goal, Reply),
        (   Reply == "true"
        ->  true
        ;   term_string(Goal, Reply),
            ignore(call(Goal))
        )
    ;   system:sleep(Seconds)
    ).
