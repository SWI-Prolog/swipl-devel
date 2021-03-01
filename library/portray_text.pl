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

SWI-Prolog has the special string data type. However, in Prolog, text
may be represented more traditionally as a list of character-codes, i.e.
(small) integers (in SWI-Prolog specifically, those are Unicode code points).
This results in output like the following (here using the backquote notation
which maps text to a list of codes):

```
?- writeln(`hello`).
[104, 101, 108, 108, 111]

?- atom_codes("hello",X).
X = [104,101,108,108,111].
```

Unless you know the Unicode tables by   heart, this is pretty unpleasant
for debugging. Loading =|library(portray_text)|=  makes the toplevel and debugger
consider certain lists of integers as text and print them as text. This
is called ``portraying''. Of course, interpretation is imperfect
as there is no way to tell in general whether [65,66] should written as
=|`AB`|= or as =|[65,66]|=. Therefore it is important that the user be aware
of the fact that this conversion is enabled. This is why this
library must be loaded explicitly.

To be able to copy the printed representation and paste it back,
printed text is enclosed in _back quotes_ if current_prolog_flag/2 for the flag
=|back_quotes|= is =|codes|= (the default), and enclosed in _double quotes_
otherwise. Certain control characters are printed out in backslash-escaped
form.

The default heuristic only considers list of codes as text if the codes are all
from the set of 7-bit ASCII without most of the control characters. A code
is classified as text by text_code/1, which in turn calls portray_text:is_text_code/1.
Define portray_text:is_text_code/1 to succeed on additional codes for more
flexibility (by default, that predicate succeeds nowhere). For example:

```
?- maplist([C,R]>>(portray_text:text_code(C)->R=y;R=n),`Générateur`,Results).
Results = [y,n,y,n,y,y,y,y,y,y].
```

Now make is_text_code/1 accept anything:

```
?- [user].
|: portray_text:is_text_code(_).
|: ^D% user://3 compiled 0.00 sec, 1 clauses
true.
```

Then:

```
?- maplist([C,R]>>(portray_text:text_code(C)->R=y;R=n),`Générateur`,Results).
Results = [y,y,y,y,y,y,y,y,y,y].
```

*/

:- dynamic
    do_portray_text/1,
    portray_text_option/2.

do_portray_text(true).

portray_text_option(min_length, 3).
portray_text_option(ellipsis,  30).

%!  portray_text(+OnOff:boolean) is det.
%
%   Switch portraying on or off. If =true=, consider lists of integers as list
%   of Unicode code points and print them as corresponding text inside
%   quotes: =|`text`|= or =|"text"|=. Quoting depends on the value of
%   current_prolog_flag/2 =|back_quotes|=.
%   This predicate can also be used to read the current portraying behaviour.
%   If OnOff is a term pull(X), then X is unified with the currently set
%   OnOff boolean.

portray_text(P) :-
    nonvar(P),
    P=pull(X),
    !,
    do_portray_text(X).
portray_text(OnOff) :-
    must_be(boolean, OnOff),
    retractall(do_portray_text(_)),
    assert(do_portray_text(OnOff)).

%!  set_portray_text(+Name, ?Value) is det.
%
%   Set options for portraying.  Options are:
%
%       * =min_length= Only consider for conversion lists of integers that
%         have a length of at least Value. Default is 3.
%       * =ellipsis= When converting a list that is longer than Value,
%         elide the output at the start using ellipsis, leaving only Value
%         number of non-elided characters: =|`...end`|=
%
%   This predicate can also be used to read the settings. If
%   Value is a term pull(X), then X is unified with the current Value
%   for the given Name.
%
%   @see '$portray_text_enabled'/1

set_portray_text(min_length, P) :-
    nonvar(P),
    P=pull(X),
    !,
    portray_text_option(min_length, X).
set_portray_text(min_length, N) :-
    nonvar(N),
    must_be(nonneg, N),
    retractall(portray_text_option(min_length, _)),
    assert(portray_text_option(min_length, N)).
set_portray_text(ellipsis, P) :-
    nonvar(P),
    P=pull(X),
    !,
    portray_text_option(ellipsis, X).
set_portray_text(ellipsis, N) :-
    nonvar(N),
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

% Idea: Maybe accept anything and hex-escape anything non-printable?
%       In particular, I could imaging 0 and ESC appearing in text of interest.
%       Currently we really accept only 7-bit ASCII so even latin-1 text
%       precludes recognition.
% Bug?: emit_code/2 can emit backspace but backspace (8) is not accepted below

text_code(Code) :-
    is_text_code(Code),
    !.
text_code(9).      % horizontal tab, \t
text_code(10).     % newline \n
text_code(13).     % carriage return \r
text_code(C) :-    % space to tilde (127 is DEL)
    between(32, 126, C).

var_or_numbered(Var) :-
    var(Var),
    !.
var_or_numbered('$VAR'(_)).

%!  is_text_code(+Code:nonneg) is semidet.
%
%   Multifile hook that can be used to extend the set of character codes
%   that is recognised as likely text. By default, is_text_code/1 fails
%   everywhere and internally, only non-control ASCII characters (32-126)
%   and the the control codes (9,10,13) are accepted.
%
%   @tbd we might be able  to  use   the  current  locale to include the
%   appropriate code page. (Does that really make sense?)


%!  '$portray_text_enabled'(-Val)
%
%   Ask the current status of  text   portraying.  Used by the graphical
%   debugger.

'$portray_text_enabled'(Val) :-
    do_portray_text(Val).


