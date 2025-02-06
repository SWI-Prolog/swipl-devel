/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
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

:- module(wasm,
          [ wasm_query_loop/0,
            wasm_abort/0,
            wasm_call_string/3,         % +String, +Input, -Output
	    wasm_call_string_with_heartbeat/4,
				        % +String, +Input, -Output, Rate
            is_object/1,                % @Term
            is_object/2,                % @Term,?Class
            (:=)/2,                     % -Result, +Call
	    await/2,			% +Request, - Result
            is_async/0,
            sleep/1,
            js_script/2,                % +String, +Options
            fetch/3,			% +URL, +Type, -Value

            op(700, xfx, :=),           % Result := Expression
            op(50,  fx,  #),            % #Value
            op(40,  yf,  [])            % Expr[Expr]
          ]).
:- autoload(library(apply), [exclude/3, maplist/3]).
:- autoload(library(terms), [mapsubterms/3]).
:- autoload(library(error), [instantiation_error/1, existence_error/2]).

:- use_module(library(uri), [uri_is_global/1, uri_normalized/3]).
:- use_module(library(debug), [debug/3]).

/** <module> WASM version support
*/

:- meta_predicate
   wasm_call_string(:, +, -),
   wasm_call_string_with_heartbeat(:, +, -, +),
   with_heartbeat(0, +).

:- create_prolog_flag(wasm_heartbeat, 10_000, [type(integer), keep(true)]).

%!  wasm_query_loop

wasm_query_loop :-
    current_prolog_flag(wasm_heartbeat, Rate),
    with_heartbeat('$toplevel':'$query_loop', Rate).

%!  wasm_abort
%
%   Execution aborted by user.

wasm_abort :-
    print_message(error, unwind(abort)),
    abort.

with_heartbeat(Goal, Rate) :-
    current_prolog_flag(heartbeat, Old),
    setup_call_cleanup(
        set_prolog_flag(heartbeat, Rate),
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
    ->  await(beat, Reply),
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

wasm_call_string_with_heartbeat(String, Input, Dict, Rate) :-
    (   var(Rate)
    ->  current_prolog_flag(wasm_heartbeat, Rate)
    ;   true
    ),
    with_heartbeat(wasm_call_string(String, Input, Dict), Rate).


%!  await(+Request, -Result) is det.
%
%   Call asynchronous behavior.  Request is normally a JavaScript
%   Promise instance.

await(Request, Result) :-
    '$await'(Request, Result0),
    (   is_dict(Result0),
        get_dict('$error', Result0, Error)
    ->  (   Error == abort
        ->  wasm_abort
        ;   throw(Error)
        )
    ;   Result = Result0
    ).


%!  is_async is semidet.
%
%   True when we can call await/2. We can  _not_ yield when we are in
%   a _callback_ from C (WASM) to Prolog.

is_async :-
    '$can_yield'.


%!  sleep(+Seconds)
%
%   Sleep by yielding when possible. Note   that this defines sleep/1 in
%   `user`, overruling system:sleep/1.

sleep(Seconds) :-
    (   is_async
    ->  Promise := prolog[promise_sleep(Seconds)],
        await(Promise, _)
    ;   system:sleep(Seconds)
    ).

%!  is_object(@Term) is semidet.
%!  is_object(@Term, ?Class) is semidet.
%
%   Test whether a Prolog term is a JavaScript object.

is_object(Term) :-
    blob(Term, js_object).

is_object(Term, Class), atom(Class) =>
    blob(Term, js_object),
    true := Term.instanceof(Class).
is_object(Term, Class), var(Class) =>
    blob(Term, js_object),
    Class := Term.instanceof().


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
    \+ Term = [_|_],
    \+ is_dict(Term).

call_func(Call, One) :-
    compound_name_arguments(Call, Pred, Args),
    maplist(call_chain, Args, Chains),
    One = _{f:Pred, args:Chains}.


:- multifile
    system:goal_expansion/2.

system:goal_expansion(In, Out) :-
    In = (_Left := _Right),
    mapsubterms(dot_list, In, Out),
    Out \== In.

dot_list(Dot, List) :-
    compound(Dot),
    compound_name_arguments(Dot,  '.', [A1, A2]),
    List = A1[A2].

%!  js_script(+String, +Options) is det.
%
%   Evaluate  String  as  JavaScript,   for  example  for  defining  a
%   function.   This  may be  used  together  with the  strings  quasi
%   quotation facility  to easily support  long strings that  may also
%   use double quotes.
%
%   ```
%   :- use_module(library(strings)).
%   :- js_script({|string||
%   function myfunc(a)
%   ...
%   |}).
%   ```
%
%   Options  is  currently   ignored.   While  this  used   to  add  a
%   ``<script>`` node to  the document it now uses  (=:)/2 to evaluate
%   the script.  I.e.  js_script is the same as:
%
%       ?- _ := eval(String).

js_script(String, _Options) :-
    _ := eval(String).


%!  user:prolog_load_file(:File, +Options) is semidet.
%
%   Hook for load_files/2 that allows loading files from URLs.

:- multifile user:prolog_load_file/2.

user:prolog_load_file(Module:File, Options) :-
    file_url(File, URL),
    load_options(URL, Options, Options1, Modified),
    (   already_loaded(URL, Modified)
    ->  '$already_loaded'(File, URL, Module, Options)
    ;   debug(load_file(true), 'Loading ~p', [URL]),
        qlf_options(URL, Type, Options1, Options2),
        fetch(URL, Type, String),
        setup_call_cleanup(
            open_string(String, In),
            load_files(Module:URL, [stream(In)|Options2]),
            close(In))
    ).

file_url(File, _), compound(File), compound_name_arity(File, _, 1) =>
    !,
    fail.                               % Alias(Path)
file_url(File, URL), atom(File), uri_is_global(File) =>
    URL = File.
file_url(File, URL), relative_path(File, Path) =>
    \+ is_absolute_file_name(Path),
    (   prolog_load_context(file, Base),
        uri_is_global(Base)
    ->  ensure_extension(Path, pl, PlPath)
    ;   \+ exists_source(Path),
        ensure_extension(Path, pl, PlPath)
    ->  Base := window.location.toString()
    ),
    uri_normalized(PlPath, Base, URL).

relative_path(Spec, Path) :-
    phrase(segments(Spec), Segments),
    atomic_list_concat(Segments, '/', Path).

ensure_extension(Path0, Ext, Path) :-
    (   file_name_extension(_, Ext0, Path0),
        user:prolog_file_type(Ext0, prolog)
    ->  Path = Path0
    ;   file_name_extension(Path0, Ext, Path)
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
    { atomic(A) },
    [A].

%!  already_loaded(+URL, +Modified) is semidet.
%
%   True when URL was already loaded.  Modified is the last change of
%   the URL content when known, unbound otherwise.

already_loaded(URL, Modified) :-
    source_file(URL),
    (   var(Modified)
    ->  debug(load_file(false), 'Already loaded (no time info) ~p', [URL])
    ;   source_file_property(URL, modified(Loaded)),
        Modified-Loaded < 1
    ->  debug(load_file(false), 'Already loaded (not modified) ~p', [URL])
    ).

load_options(URL, Options, [modified(Modified)|Options], Modified) :-
    url_properties(URL, Properties),
    (   200 = Properties.get(status)
    ->  true
    ;   existence_error(url, URL)
    ),
    Modified = Properties.get(last_modified),
    Modified > 0,
    !.
load_options(_, Options, [modified(Now)|Options], _) :-
    get_time(Now).

qlf_options(URL, blob, Options, [format(qlf)|Options]) :-
    file_name_extension(_, Ext, URL),
    user:prolog_file_type(Ext, qlf),
    !.
qlf_options(_, text, Options, Options).


%!  http(+URL, +Action, -Result)
%
%   Implement the file access protocol for URLs.
%
%   @tbd requires the ability to  yield   from  the  callbacks that hook
%   these predicates into the file access   primitives. The hook must be
%   defined not to get errors on unknown iri scheme.

:- if(true).
http(_,_,_) :- !, fail.
:- else.
http(open(read, _Options), URL, In) :-
    fetch(URL, text, String),
    open_string(String, In).
http(read, URL, Bool) :-
    url_properties(URL, Properties),
    (   Properties.status == 200
    ->  Bool = true
    ;   Bool = false
    ).
http(time, URL, Time) :-
    url_properties(URL, Properties),
    (   Time = Properties.get(time),
        Time > 0
    ).
http(size, URL, Size) :-
    url_properties(URL, Properties),
    (   Size = Properties.get(size),
        Size >= 0
    ).
:- endif.

:- register_iri_scheme(http,  http, []).
:- register_iri_scheme(https, http, []).

%!  url_properties(+URL, -Properties:dict) is det.
%
%   Asynchronously fetch properties for  URL using a ``HEAD`` request.
%   Properties contains the keys `url`, `status` and on success `size`
%   and `last_modified`.  We  cache the result for at  least 5 seconds
%   or 20 times the time to fetch it.

:- dynamic
       url_property_cache/3.

url_properties(URL, Properties) :-
    url_property_cache(URL, Properties, Expire),
    get_time(Now),
    (   Now < Expire
    ->  !
    ;   retractall(url_property_cache(URL, _, _)),
        fail
    ).
url_properties(URL, Properties) :-
    Promise := prolog.url_properties(#URL),
    get_time(Start),
    await(Promise, Properties),
    get_time(Now),
    Expire is Now + max(5, (Now-Start)*20),
    asserta(url_property_cache(URL, Properties, Expire)).

%!  fetch(+URL, +Type, -Data) is det.
%
%   Fetch the content from URL asynchronously. Type  is a method name on
%   the Response object  returned  by   fetch(),  e.g.,  `text`, `json`,
%   `html`, `blob`.

fetch(URL, As, Data) :-
    Promise := prolog.fetch(#URL, _{cache: 'no-cache'}, #As),
    await(Promise, Data0),
    (   As == blob
    ->  P2 := Data0.arrayBuffer(),
        await(P2, Data)
    ;   Data = Data0
    ).


%!  prolog:confirm(+Message, -Boolean) is semidet.
%
%   Conform  some  action.   Currently uses  the  browser's  confirm()
%   method.

:- multifile
       prolog:confirm/2.

prolog:confirm(Message, Result) :-
    message_to_string(Message, String),
    Result := window.confirm(String).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:message//1.

prolog:message(JsError) -->
    { is_object(JsError),
      Msg := JsError.toString()
    },
    [ 'JavaScript: ~w'-[Msg] ].
prolog:error_message(js_error(Msg)) -->
    [ 'JavaScript: ~w'-[Msg] ].
