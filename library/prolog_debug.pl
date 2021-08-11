/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2021, SWI-Prolog Solutions b.v.
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

:- module(prolog_debug,
          [ spy/1,                  % :Spec
            nospy/1,                % :Spec
            nospyall/0,
            debugging/0
          ]).

/** <module> User level debugging tools

This  library  provides  tools   to    control   the  Prolog  debuggers.
Traditionally this code was  built-in.  Because   these  tools  are only
required in (interactive) debugging sessions they   have been moved into
the library.
*/

%!  prolog:debug_control_hook(+Action)
%
%   Allow user-hooks in the Prolog debugger interaction.  See the calls
%   below for the provided hooks.  We use a single predicate with action
%   argument to avoid an uncontrolled poliferation of hooks.

:- multifile
    prolog:debug_control_hook/1.    % +Action

:- meta_predicate
    spy(:),
    nospy(:).

%!  spy(:Spec) is det.
%!  nospy(:Spec) is det.
%!  nospyall is det.
%
%   Set/clear spy-points. A successfully set or cleared spy-point is
%   reported using print_message/2, level  =informational=, with one
%   of the following terms, where Spec is of the form M:Head.
%
%       - spy(Spec)
%       - nospy(Spec)
%
%   @see    spy/1 and nospy/1 call the hook prolog:debug_control_hook/1
%           to allow for alternative specifications of the thing to
%           debug.

spy(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
spy(_:[]) :- !.
spy(M:[H|T]) :-
    !,
    spy(M:H),
    spy(M:T).
spy(Spec) :-
    notrace(prolog:debug_control_hook(spy(Spec))),
    !.
spy(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
        pi_to_head(PI, Head),
        '$define_predicate'(Head),
        '$spy'(Head),
    fail.
spy(_).

nospy(_:X) :-
    var(X),
    throw(error(instantiation_error, _)).
nospy(_:[]) :- !.
nospy(M:[H|T]) :-
    !,
    nospy(M:H),
    nospy(M:T).
nospy(Spec) :-
    notrace(prolog:debug_control_hook(nospy(Spec))),
    !.
nospy(Spec) :-
    '$find_predicate'(Spec, Preds),
    '$member'(PI, Preds),
         pi_to_head(PI, Head),
        '$nospy'(Head),
    fail.
nospy(_).

nospyall :-
    notrace(prolog:debug_control_hook(nospyall)),
    fail.
nospyall :-
    spy_point(Head),
        '$nospy'(Head),
    fail.
nospyall.

pi_to_head(M:PI, M:Head) :-
    !,
    pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
    functor(Head, Name, Arity).

%!  debugging is det.
%
%   Report current status of the debugger.

debugging :-
    notrace(prolog:debug_control_hook(debugging)),
    !.
debugging :-
    current_prolog_flag(debug, true),
    !,
    print_message(informational, debugging(on)),
    findall(H, spy_point(H), SpyPoints),
    print_message(informational, spying(SpyPoints)).
debugging :-
    print_message(informational, debugging(off)).

spy_point(Module:Head) :-
    current_predicate(_, Module:Head),
    '$get_predicate_attribute'(Module:Head, spy, 1),
    \+ predicate_property(Module:Head, imported_from(_)).
