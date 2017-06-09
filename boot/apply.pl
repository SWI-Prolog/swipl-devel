/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2008, University of Amsterdam
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

:- module('$apply',
          [ forall/2,                   % :Goal, :Goal
            apply/2                     % :Goal, +ExtraArgs
          ]).

:- meta_predicate
    forall(0,0),
    apply(:, +).

:- noprofile((forall/2, apply/2)).

%!  forall(+Condition, +Action)
%
%   True if Action if true for all variable bindings for which Condition
%   if true.

forall(Cond, Action) :-
    \+ (Cond, \+ Action).

%!  apply(:Goal, +ExtraArgs) is nondet.
%
%   Extend Goal with arguments from ExtraArgs and call it.
%
%   @deprecated     Almost all usage can be replaced by call/N.

apply(M:Name, Extra) :-
    atom(Name),
    !,
    compound_name_arguments(G, Name, Extra),
    call(M:G).
apply(M:Goal, Extra) :-
    compound(Goal),
    !,
    compound_name_arguments(Goal, Name, Args0),
    '$append'(Args0, Extra, Args),
    compound_name_arguments(G, Name, Args),
    call(M:G).
apply(_:Goal, _Extra) :-
    '$type_error'(callable, Goal).
