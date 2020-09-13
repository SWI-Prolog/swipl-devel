/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2020, University of Amsterdam
                              CWI, Amsterdam
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

:- module(portray_text,
          [ portray_text/1,             % +Bool
            set_portray_text/2,         % +Name, +Value

            '$portray_text_enabled'/1
          ]).
:- autoload(library(error),[must_be/2]).

:- multifile
    is_text_code/1.                     % +Integer

/** <module> Portray text

A Prolog string is a list of character-codes: (small) integers, which
results in output like this:

    ==
    ?- writeln(`hello`).
    [104, 101, 108, 108, 111]
    ==

Unless you know the Unicode tables by   heart, this is pretty unpleasant
for debugging. Loading this library  makes   the  toplevel  and debugger
print strings with at least 3 characters  as "text ...". Of course, this
is an ambiguous operation because nobody can know whether [65,66] should
be written as "AB" or a list: to  Prolog they are the same. Therefore it
is imported that the user  is  aware   of  the  fact that this heuristic
conversion  is  enabled.  This  is  why  this  library  must  be  loaded
explicitly to enable this conversion.

@tbd    Allow setting the character-codes we try to convert
*/

:- dynamic
    do_portray_text/1,
    portray_text_option/2.

do_portray_text(true).

portray_text_option(min_length, 3).
portray_text_option(ellipsis,  30).

%!  portray_text(+Boolean) is det.
%
%   If =true=, write lists of character   codes as "..." to simplify
%   debugging.

portray_text(OnOff) :-
    must_be(boolean, OnOff),
    retractall(do_portray_text(_)),
    assert(do_portray_text(OnOff)).

%!  set_portray_text(+Name, +Value) is det.
%
%   Set options for writing lists as strings.  Options are
%
%       * min_length
%       Only consider lists that are at least this long
%       * ellipsis
%       Write strings that are longer as "start...end"

set_portray_text(min_length, N) :-
    must_be(nonneg, N),
    retractall(portray_text_option(min_length, _)),
    assert(portray_text_option(min_length, N)).
set_portray_text(ellipsis, N) :-
    must_be(nonneg, N),
    retractall(portray_text_option(ellipsis, _)),
    assert(portray_text_option(ellipsis, N)).


:- multifile
    user:portray/1.
:- dynamic
    user:portray/1.

user:portray(Codes) :-
    do_portray_text(true),
    '$skip_list'(Length, Codes, _Tail),
    portray_text_option(min_length, MinLen),
    Length >= MinLen,
    all_codes(Codes),
    portray_text_option(ellipsis, IfLonger),
    quote(C),
    put_code(C),
    (   Length > IfLonger
    ->  First is IfLonger - 5,
        Skip is Length - 5,
        skip_first(Skip, Codes, Rest),
        put_n_codes(First, Codes, C),
        format('...', [])
    ;   Rest = Codes
    ),
    put_var_codes(Rest, C),
    put_code(C).

quote(0'`) :-
    current_prolog_flag(back_quotes, codes),
    !.
quote(0'").

put_n_codes(N, [H|T], C) :-
    N > 0,
    !,
    emit_code(H, C),
    N2 is N - 1,
    put_n_codes(N2, T, C).
put_n_codes(_, _, _).

skip_first(N, [_|T0], T) :-
    succ(N2, N),
    !,
    skip_first(N2, T0, T).
skip_first(_, L, L).

put_var_codes(Var, _) :-
    var_or_numbered(Var),
    !,
    format('|~p', [Var]).
put_var_codes([], _).
put_var_codes([H|T], C) :-
    emit_code(H, C),
    put_var_codes(T, C).

emit_code(Q, Q)    :- !, format('\\~c', [Q]).
emit_code(0'\b, _) :- !, format('\\b').
emit_code(0'\r, _) :- !, format('\\r').
emit_code(0'\n, _) :- !, format('\\n').
emit_code(0'\t, _) :- !, format('\\t').
emit_code(C, _) :- put_code(C).

all_codes(Var) :-
    var_or_numbered(Var),
    !.
all_codes([]).
all_codes([H|T]) :-
    is_code(H),
    all_codes(T).

is_code(Term) :-
    integer(Term),
    Term >= 0,
    text_code(Term),
    !.

text_code(Code) :-
    is_text_code(Code),
    !.
text_code(9).
text_code(10).
text_code(13).                         % ok ...
text_code(C) :-
    between(32, 126, C).

var_or_numbered(Var) :-
    var(Var),
    !.
var_or_numbered('$VAR'(_)).

%!  is_text_code(+Code:nonneg) is semidet.
%
%   Multifile hook that can be used to extend the set of character codes
%   that is recognised as likely  text.   Default  are non-control ASCII
%   characters (9,10,13,32-126)
%
%   @tbd we might be able  to  use   the  current  locale to include the
%   appropriate code page.


%!  '$portray_text_enabled'(-Val)
%
%   Ask the current status of  text   portraying.  Used by the graphical
%   debugger.

'$portray_text_enabled'(Val) :-
    do_portray_text(Val).
