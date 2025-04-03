/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

:- module(dom,
          [ bind/4,                     % +Elem, +EventType, -Event, :Goal
            bind_async/4,               % +Elem, +EventType, -Event, :Goal
            unbind/2,                   % +Elem, +EventType
            wait/3                      % +Elem, +EventType, =Event
          ]).
:- use_module(wasm).

:- meta_predicate
    bind(+,+,-,0),
    bind_async(+,+,-,0).

/** <module> Browser DOM manipulation

@see library(dialect/tau/dom).
*/

                /*******************************
                *        EVENT HANDLING        *
                *******************************/

%!  bind(+Elem, +EventType, -Event, :Goal) is det.
%!  bind_async(+Elem, +EventType, -Event, :Goal) is det.
%
%   Bind EventType on Elem to call Goal. If  Event appears in Goal is is
%   bound to the current  event.
%
%   The bind_async/4 variation runs the event   handler  on a new Prolog
%   _engine_ using Prolog.forEach().  This implies that the handler runs
%   asynchronously and all its solutions are enumerated.
%
%   @compat bind_async/5 is a SWI-Prolog extension to the Tau library

bind(Elem, On, Ev, Goal) :-
    bind(Elem, On, Ev, Goal, #{}).

bind_async(Elem, On, Ev, Goal) :-
    bind(Elem, On, Ev, Goal, #{async:true}).

bind(Elem, On, Ev, Goal, Options) :-
    foldsubterms(map_object, Goal, Goal1, t(1,[],[]), t(_,VarNames,Map)),
    Map \== [],
    dict_pairs(Input, #, Map),
    term_string(Goal1, String, [variable_names(['Event__'=Ev|VarNames])]),
    _ := prolog.bind(Elem, #On, String, Input, Options).
bind(Elem, On, Ev, Goal, Options) :-
    term_string(Goal, String, [variable_names(['Event__'=Ev])]),
    _ := prolog.bind(Elem, #On, String, Options).

map_object(Obj, Var, t(N0,VN,Map), t(N,[VarName=Var|VN], [VarName-Obj|Map])) :-
    is_object(Obj),
    N is N0+1,
    format(atom(VarName), 'JsObject__~d__', [N0]).

%!  unbind(+Elem, +EventType) is det.
%
%   Remove the event listener for EventType.

unbind(Elem, EventType) :-
    _ := Elem.removeEventListener(#EventType).

%!  unbind(+Elem, +EventType, :Goal) is det.
%
%   Remove the event listener for EventType that executes Goal.
%   @tbd Implement.  How do we do this?  We somehow need to be
%   able to find the function from Goal.

%!  wait(+Elem, +EventType, =Event) is det.
%
%   Make the calling task wait for EventType   on  Elem. If the event is
%   triggered, Event is unified with the event object.

wait(Elem, EventType, Event) :-
    must_be_async(wait/3),
    Promise := prolog.promise_event(Elem, #EventType),
    await(Promise, Event).
