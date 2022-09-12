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
	    wasm_call_string_with_heartbeat/3,
				        % +String, +Input, -Output
            (:=)/2,                     % -Result, +Call
            sleep/1,
            js_script/2,                % +String, +Options

            op(700, xfx, :=),           % Result := Expression
            op(40,  fx,  #),            % #Value
            op(50,  yf,  [])            % Expr[Expr]
          ]).
:- autoload(library(apply), [exclude/3, maplist/3]).
:- autoload(library(terms), [mapsubterms/3]).

/** <module> WASM version support
*/

:- meta_predicate
   wasm_call_string(:, +, -),
   wasm_call_string_with_heartbeat(:, +, -),
   with_heartbeat(0).

%!  wasm_query_loop

wasm_query_loop :-
    with_heartbeat('$toplevel':'$query_loop').

%!  wasm_abort
%
%   Execution aborted by userthe

wasm_abort :-
    print_message(error, '$aborted'),
    abort.

with_heartbeat(Goal) :-
    current_prolog_flag(heartbeat, Old),
    setup_call_cleanup(
        set_prolog_flag(heartbeat, 10 000),
	call(Goal),
        set_prolog_flag(heartbeat, Old)).

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

wasm_call_string_with_heartbeat(String, Input, Dict) :-
    with_heartbeat(wasm_call_string(String, Input, Dict)).


%!  sleep(+Seconds)
%
%   Sleep by yielding when possible. Note   that this defines sleep/1 in
%   `user`, overruling system:sleep/1.

sleep(Seconds) :-
    (   '$can_yield'
    ->  js_yield(_{command:sleep, time:Seconds}, Reply),
        term_string(Goal, Reply),
        (   Reply == "true"
        ->  true
        ;   term_string(Goal, Reply),
            ignore(call(Goal))
        )
    ;   system:sleep(Seconds)
    ).

%!  :=(-Result, +Call) is det.
%!  :=(+Target, +Value) is det.
%
%   Call a JavaScript function expressed by   Call.  Call is a compound.
%   The functor name denotes the function to be called and the arguments
%   are converted using `Prolog.toJSON`. The   function return value can
%   be accessed using js_call(Return = Call).   In this case `Return` is
%   the return value of the function converted by `Prolog.toProlog()`.
%   Examples:
%
%	?- Res := myfunc([1,2,3]).
%	?- Max := 'Math'.max(10, 20).
%	?- Out := document.getElementById('output').
%	?- Par := document.createElement(p),
%          Par.textContent := #Text.
%       ?- Par.textContent := "aap" + " " + "noot".

On[Setter] := Value, atom(Setter) =>
    call_chain(On, TargetChain),
    call_chain(Value, ValueChain),
    '$js_call'(_{ setter:Setter,
                  target:TargetChain,
                  value:ValueChain
                }, _Result).
Result := Call =>
    call_chain(Call, Chain),
    '$js_call'(Chain, Result).

%!  call_chain(+Callers, -Chain) is det.
%
%   Represent a chain of calls as  `obj.getter.f(x)   ...`  as a list of
%   objects. Each object in the list is either  an atom (for a getter or
%   the first global variable) or a callable term represented as
%   `{ f: Name, args: Args }`.

call_chain(Calls, Chain) :-
    call_chain(Calls, Chain, []).

call_chain(On[Call], Chain, Tail) =>
    call_chain(On, Chain, Tail0),
    call1(Call, Next),
    Tail0 = [Next|Tail].
call_chain(First, Chain, Tail) =>
    call_first(First, Next),
    Chain = [Next|Tail].

call1(Getter, One), atom(Getter) =>
    One = Getter.
call1(Call, One), is_func(Call) =>
    call_func(Call, One).

call_first(#Value, One) =>
    One = _{v:Value}.
call_first(Getter, One), atom(Getter) =>
    One = Getter.
call_first(First, One), is_func(First) =>
    call_func(First, One).
call_first(Obj, One) =>
    One = _{v:Obj}.

is_func(Term) :-
    compound(Term),
    \+ is_dict(Term).

call_func(Call, One) :-
    compound_name_arguments(Call, Pred, Args),
    maplist(call_chain, Args, Chains),
    One = _{f:Pred, args:Chains}.


:- multifile
    user:goal_expansion/2.

user:goal_expansion(In, Out) :-
    In = (_Left := _Right),
    mapsubterms(dot_list, In, Out),
    Out \== In.

dot_list(Dot, List) :-
    compound(Dot),
    compound_name_arguments(Dot,  '.', [A1, A2]),
    List = A1[A2].

%!  js_script(+String, +Options) is det.
%
%   Add a JavaScript script node to the  document body. For long strings
%   one can use a `string` quasi quotation, e.g.
%
%   ```
%   :- use_module(library(strings)).
%   :- js_string({|string||
%   function myfunc(a)
%   ...
%   |}).
%   ```
%
%   Options:
%
%     - id(Atom)
%       Node identifier.  Using the same id replaces the script rather
%       than adding a new one.

js_script(String, Options) :-
    dict_options(Dict, Options),
    _ := js_add_script(String, Dict).
