/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2000-2011, University of Amsterdam
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

:- module(checkselect,
          [ check_old_select/0
          ]).

/** <module> Check usage of old select/3

This module simplifies porting 3.3.x   applications  using select/3 with
the wrong argument order to 3.4.
*/

%!  check_old_select
%
%   When compiling, print calls to select/3 that may use the wrong
%   argument order.  Upto version 3.3.x the argument order of select/3
%   as
%
%           select(+List, ?Element, ?RestList).
%
%   Later versions use the compatible version
%
%           select(?Element, +List, ?RestList).

check_old_select :-
    print_message(informational, select_check).

user:goal_expansion(select(L,E,R), _) :-
    print_message(warning, select_arguments(select(L,E,R))),
    fail.

                 /*******************************
                 *      CHECKING VERSION        *
                 *******************************/

:- redefine_system_predicate(user:select(_,_,_)).

user:select(E, L, R) :-
    \+ is_list(L),
    print_message(error, select_call(select(E,L,R))),
    trace,
    fail.
user:select(E, [E|R], R).
user:(select(E, [H|T], [H|R]) :-
        select(E, T, R)).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(select_check) -->
    [ 'Enabled checking for wrong argument order in select/3' ].
prolog:message(select_arguments(S)) -->
    { numbervars(S, 0, _)
    },
    [ 'Select/3 used; check argument order: ~p'-[S] ].
prolog:message(select_call(S)) -->
    [ 'Suspicious select/3 call, entering debugger: ~p'-[S] ].

