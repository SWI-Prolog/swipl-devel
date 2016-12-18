/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2003-2011, University of Amsterdam
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

:- module(checklast,
          [ check_old_last/0
          ]).
:- use_module(library(lists)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module simplifies porting < 5.1.11   applications using last/2 with
the wrong argument order to >=  5.1.12.   For  further discussion on the
argument order of last/2, see

http://www.prolog-standard.fmg.uva.nl/twiki/bin/view/Library/PredLast2
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

check_old_last :-
    print_message(informational, last_check).

user:goal_expansion(last(L,E), _) :-
    print_message(warning, last_arguments(last(L,E))),
    fail.

                 /*******************************
                 *      CHECKING VERSION        *
                 *******************************/

:- abolish(lists:last/2).

%!  last(?List, ?Elem)
%
%   Succeeds if `Last' unifies with the last element of `List'.

lists:last(List, Last) :-
    \+ is_list(List),
    !,
    print_message(error, last_call(last_call(List, Last))),
    trace,
    fail.
lists:last([X|Xs], Last) :-
    last_(Xs, X, Last).

last_([], Last, Last).
last_([X|Xs], _, Last) :-
    last_(Xs, X, Last).


                 /*******************************
                 *           MESSAGES           *
                 *******************************/

:- multifile
    prolog:message/3.

prolog:message(last_check) -->
    [ 'Enabled checking for wrong argument order in last/2' ].
prolog:message(last_arguments(S)) -->
    { numbervars(S, 0, _)
    },
    [ 'Last/2 used; check argument order: ~p'-[S] ].
prolog:message(last_call(S)) -->
    [ 'Suspicious last/2 call, entering debugger: ~p'-[S] ].

