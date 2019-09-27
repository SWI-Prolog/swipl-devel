/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, VU University Amsterdam
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

:- module(standard,
          [ error_write/1,              % @Message
            error_writeln/1,            % @Message
            console_write/1,            % @Message
            console_writeln/1,          % @Message
            warning/1,                  % @Message
            message/1,                  % @Message
            messageln/1                 % @Message
          ]).

/** <module> XSB Term Writing to Designated I/O Streams

This module emulates the XSB dedicated  term writing predicates from the
`standard` module.

@compat XSB has a number of additional streams which we do not have. For
now we send all messages to   `user_error`. Onlt warning/1 is redirected
through SWI-Prolog's print_message/2 interface.
*/

:- create_prolog_flag(warning_action, print_warning, [keep(true)]).

%!  error_write(@Term) is det.
%!  error_writeln(@Term) is det.
%
%   As write/1 and writeln/1 to `user_error`.

error_write(Term) :-
    write(user_error, Term).
error_writeln(Term) :-
    writeln(user_error, Term).

%!  console_write(@Term) is det.
%!  console_writeln(@Term) is det.
%
%   As write/1 and writeln/1 to `user_error`.  The XSB version writes to
%   ``STDFDBK``.  What does that mean?

console_write(Term) :-
    write(user_error, Term).
console_writeln(Term) :-
    writeln(user_error, Term).

%!  warning(@Message) is det.
%
%   Print a warning. The behaviour depends on the flag `warning_action`,
%   which can be one of:
%
%     - print_warning
%       Re-direct the message to SWI-Prolog's print_message
%     - error_warning
%       Throw error(xsb_warning(Message), _)
%     - silent_warning
%       Ignore the warning.
%
%   If Message is a list or   comma-list,  the elements are concatenated
%   without a space.

warning(Message) :-
    current_prolog_flag(warning_action, Action),
    warning(Action, Message).

warning(silent_warning, _) :-
    !.
warning(error_warning, Term) :-
    !,
    throw(error(xsb_warning(Term), _)).
warning(_, Term) :-
    print_message(warning, xsb_warning(Term)).

:- multifile
    prolog:message//1.

prolog:message(xsb_warning(Term)) -->
    [ 'XSB:'-[] ],
    xsb_warning(Term).
prolog:error_message(xsb_warning(Term)) -->
    [ 'XSB:'-[] ],
    xsb_warning(Term).

xsb_warning(List) -->
    { is_list(List) },
    !,
    xsb_warning_list(List).
xsb_warning(Var) -->
    { var(Var) },
    !,
    [ '~w'-[Var] ].
xsb_warning((A,B)) -->
    !,
    xsb_warning(A),
    xsb_warning(B).
xsb_warning(Term) -->
    [ '~w'-[Term] ].

xsb_warning_list([]) -->
    [].
xsb_warning_list([H|T]) -->
    [ '~w'-[H] ],
    xsb_warning_list(T).

%!  message(@Message) is det.
%!  messageln(@Message) is det.
%
%   Write message to `user_error`. As warning/1,   Message can be a list
%   or comma list.
%
%   @compat XSB. XSB writes  to  ``STDMSG``.   Possibly  we  should also
%   redirect this through SWI-Prolog's print_message/2 interface.

message(Message) :-
    is_list(Message),
    !,
    maplist(message_elem, Message).
message(Var) :-
    var(Var),
    !,
    message_elem(Var).
message((A,B)) :-
    !,
    message(A),
    message(B).
message(Term) :-
    message_elem(Term).

messageln(Term) :-
    message(Term),
    nl(user_error).

message_elem(Term) :-
    write(user_error, Term).

