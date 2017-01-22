/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2011, University of Amsterdam
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
            set_portray_text/2          % +Name, +Value
          ]).
:- use_module(library(error)).

/** <module> Portray text

A Prolog string is a list of character-codes: (small) integers, which
results in output like this:

    ==
    ?- writeln("hello").
    [104, 101, 108, 108, 111]
    ==

Unless you know the Unicode tables by   heart, this is pretty unpleasant
for debugging. Loading this library  makes   the  toplevel  and debugger
print strings with at least 3 characters  as "text ...". Of course, this
is an ambigious operation because nobody can know whether [65,66] should
be written as "AB" or a list: to  Prolog they are the same. Therefore it
is imported that the user  is  aware   of  the  fact that this heuristic
conversion  is  enabled.  This  is  why  this  library  must  be  loaded
explicitely to enable this conversion.

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
    '$skip_list'(Length, Codes, Tail),
    portray_text_option(min_length, MinLen),
    Length >= MinLen,
    all_ascii(Codes),
    portray_text_option(ellipsis, IfLonger),
    put_char('"'),
    (   Length > IfLonger
    ->  First is IfLonger - 5,
        Skip is Length - 5,
        skip_first(Skip, Codes, Rest),
        put_n_codes(First, Codes),
        format('...', [])
    ;   Rest = Codes
    ),
    (   var_or_numbered(Tail)
    ->  put_var_codes(Rest)
    ;   format('~s', [Rest])
    ),
    put_char('"').

put_n_codes(N, [H|T]) :-
    N > 0,
    !,
    emit_code(H),
    N2 is N - 1,
    put_n_codes(N2, T).
put_n_codes(_, _).

skip_first(N, [_|T0], T) :-
    succ(N2, N),
    !,
    skip_first(N2, T0, T).
skip_first(_, L, L).

put_var_codes(Var) :-
    var_or_numbered(Var),
    !,
    format('|~p', [Var]).
put_var_codes([]).
put_var_codes([H|T]) :-
    emit_code(H),
    put_var_codes(T).

emit_code(0'\b) :- !, format('\\b').
emit_code(0'\r) :- !, format('\\r').
emit_code(0'\n) :- !, format('\\n').
emit_code(0'\t) :- !, format('\\t').
emit_code(C) :- put_code(C).

all_ascii(Var) :-
    var_or_numbered(Var),
    !.
all_ascii([]).
all_ascii([H|T]) :-
    isascii(H),
    all_ascii(T).

isascii(Term) :-
    integer(Term),
    ascii_code(Term),
    !.

ascii_code(9).
ascii_code(10).
ascii_code(13).                         % ok ...
ascii_code(C) :-
    between(32, 126, C).

var_or_numbered(Var) :-
    var(Var),
    !.
var_or_numbered('$VAR'(_)).
