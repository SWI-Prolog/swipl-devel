/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2013, University of Amsterdam
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

:- module(broadcast,
          [ listen/3,           % Listener x Templ x Goal
            listen/2,           % Templ x Goal
            unlisten/1,         % Listener
            unlisten/2,         % Listener x Templ
            unlisten/3,         % Listener x Templ x Goal
            listening/3,        % Listener x Templ x Goal
            broadcast/1,        % Templ
            broadcast_request/1 % Templ
          ]).
:- meta_predicate
    listen(+, 0),
    listen(+, +, 0),
    unlisten(+, +, 0).

:- dynamic
    listener/4.

/** <module> Event service

Generic broadcasting service. Broadcasts are   made  using the predicate
broadcast(+Templ). All registered  `listeners'  will   have  their  goal
called. Success or failure of this is ignored. The listener can not bind
arguments.

This library is particulary  useful  for   disconnecting  modules  in an
application. Modules can broadcast events  such as changes, anticipating
other modules need to react on   such  changes. For example, settings.pl
broadcasts changes to settings, allowing dependent   modules to react on
changes:

==
:- listing(setting(changed(http:workers, New)),
           change_workers(New)).

change_workers(New) :-
        setting(http:port, Port),
        http_workers(Port, New).
==
*/

%!  listen(+Listener, +Templ, :Goal) is det.
%!  listen(+Templ, :Goal) is det.
%
%   Open a channel for listening for events of the given `Templ'.

listen(Listener0, Templ, Module:Goal) :-
    canonical_listener(Listener0, Listener),
    assert_listener(Templ, Listener, Module, Goal).

listen(Templ, Module:Goal) :-
    assert_listener(Templ, Module, Module, Goal).


%!  unlisten(+Listener) is det.
%!  unlisten(+Listener, +Templ) is det.
%!  unlisten(+Listener, +Templ, :Goal) is det.
%
%   Destroy a channel. All arguments may  be variables, removing the
%   all matching listening channals.

unlisten(Listener0) :-
    canonical_listener(Listener0, Listener),
    retractall(listener(_, Listener, _, _)).
unlisten(Listener0, Templ) :-
    canonical_listener(Listener0, Listener),
    retractall(listener(Templ, Listener, _, _)).
unlisten(Listener0, Templ, Module:Goal) :-
    canonical_listener(Listener0, Listener),
    retract_listener(Templ, Listener, Module, Goal).


%!  listening(?Listener, ?Templ, ?Goal) is nondet.
%
%   returns currently open channels

listening(Listener0, Templ, Module:Goal) :-
    canonical_listener(Listener0, Listener),
    listener(Templ, Listener, Module, Goal).


%!  broadcast(+Templ) is det.
%
%   Broadcast given event.

broadcast(Templ) :-
    (   listener(Templ, _Listener, Module, Goal),
        (   Module:Goal
        ->  fail
        )
    ;   true
    ).


%!  broadcast_request(+Templ) is nondet.
%
%   Broadcast given event till accepted.  Succeeds then, fail if no
%   listener accepts the call.  Bindings made by the listener goal
%   are maintained.  May be used to make broadcast requests.

broadcast_request(Templ) :-
    listener(Templ, _Listener, Module, Goal),
    Module:Goal.


%       {assert,retract}_listener(+Templ, +Listener, +Module, +Goal)
%
%       Implemented as sub-predicate to ensure storage in this module.
%       Second registration is ignored.  Is this ok?  It avoids problems
%       using multiple registration of global listen channels.

assert_listener(Templ, Listener, Module, TheGoal) :-
    listener(Templ, Listener, Module, TheGoal),
    !.
assert_listener(Templ, Listener, Module, TheGoal) :-
    asserta(listener(Templ, Listener, Module, TheGoal)).

retract_listener(Templ, Listener, Module, TheGoal) :-
    retractall(listener(Templ, Listener, Module, TheGoal)).

%!  canonical_listener(+Raw, -Canonical)
%
%   Entry for later optimization.

canonical_listener(Templ, Templ).

