/*  Part of SWI-Prolog

    Author:        Michael Hendricks
    E-mail:        michael@ndrix.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2013,2014, Michael Hendricks
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:

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
    COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(prolog_format,
	  [ format_spec/2,			% +Format, -Spec
	    format_spec//1,			% -Spec
	    format_types/2			% +Format, -Types
	  ]).
:- use_module(library(dcg/basics), [eos//0, integer//1, string_without//2]).
:- use_module(library(when), [when/2]).

/** <module> Analyse format specifications

This library parses the format specification  used by format/1, format/2
and format/3. The parsed  specification  can   be  used  to validate the
consistency of the  format  string  and   the  provided  arguments.  For
example:

  ==
  ?- format_types('~d bottles of beer', Types).
  Types = [integer].
  ==

@tbd	The current implementation does not support format_predicate/2.
@see	http://www.swi-prolog.org/pack/list?p=format_spec
@author	Michael Hendricks
*/

%%  format_spec(+Format, -Spec:list) is semidet.
%
%   Parse a format string. Each element of Spec is one of the following:
%
%    * text(Text)
%    Text sent to the output as is
%    * escape(Num,Colon,Action)
%    A format escape. Num represents the optional numeric portion of
%    an esape. Colon represents the optional colon in an escape.
%    Action is an atom representing the action to be take by this
%    escape.

format_spec(Format, Spec) :-
    when((ground(Format);ground(Codes)),text_codes(Format, Codes)),
    once(phrase(format_spec(Spec), Codes, [])).

%%  format_spec(-Spec)//
%
%   DCG for parsing format  strings.  It   doesn't  yet  generate format
%   strings from a spec. See format_spec/2 for details.

format_spec([]) -->
    eos.
format_spec([escape(Numeric,Modifier,Action)|Rest]) -->
    "~",
    numeric_argument(Numeric),
    modifier_argument(Modifier),
    action(Action),
    format_spec(Rest).
format_spec([text(String)|Rest]) -->
    { when((ground(String);ground(Codes)),string_codes(String, Codes)) },
    string_without("~", Codes),
    { Codes \= [] },
    format_spec(Rest).

%%  format_types(+Format:text, -Types:list) is det.
%
%   True when Format requires an argument list   with  terms of the type
%   specified by Types. The  length  of  this   list  is  the  number of
%   arguments required. Each value of Types is   a  type as described by
%   error:has_type/2.

format_types(Format, Types) :-
    format_spec(Format, Spec),
    spec_types(Spec, Types).

%%  spec_types(+FormatSpec, -Types:list(type)) is det.
%
%   True if FormatSpec requires format/2  to   have  arguments of Types.
%   Each value of Types is a type as described by error:has_type/2. This
%   notion of types is compatible with library(mavis).

spec_types(Spec, Types) :-
    phrase(spec_types(Spec), Types).

spec_types([]) -->
    [].
spec_types([Item|Items]) -->
    item_types(Item),
    spec_types(Items).

item_types(text(_)) -->
    [].
item_types(escape(Numeric,_,Action)) -->
    numeric_types(Numeric),
    action_types(Action).

numeric_types(number(_)) -->
    [].
numeric_types(character(_)) -->
    [].
numeric_types(star) -->
    [number].
numeric_types(nothing) -->
    [].

action_types(Action) -->
    { atom_codes(Action, [Code]) },
    { action_types(Code, Types) },
    phrase(Types).

%% text_codes(Text:text, Codes:codes).
text_codes(Var, Codes) :-
    var(Var),
    !,
    string_codes(Var, Codes).
text_codes(Atom, Codes) :-
    atom(Atom),
    !,
    atom_codes(Atom, Codes).
text_codes(String, Codes) :-
    string(String),
    !,
    string_codes(String, Codes).
text_codes(Codes, Codes) :-
    is_of_type(codes, Codes).


numeric_argument(number(N)) -->
    integer(N).
numeric_argument(character(C)) -->
    "`",
    [C].
numeric_argument(star) -->
    "*".
numeric_argument(nothing) -->
    "".

modifier_argument(colon) -->
    ":".
modifier_argument(no_colon) -->
    \+ ":".

action(Action) -->
    [C],
    { is_action(C) },
    { atom_codes(Action, [C]) }.

%%  is_action(+Action:integer) is semidet.
%%  is_action(-Action:integer) is multi.
%
%   True if Action is a valid   format/2  action character. Iterates all
%   acceptable action characters, if Action is unbound.

is_action(Action) :-
    action_types(Action, _).

%%  action_types(?Action:integer, ?Types:list(type))
%
%   True if Action consumes arguments matching   Types.  An action (like
%   `~`), which consumes no arguments, has `Types=[]`. For example,
%
%       ?- action_types(0'~, Types).
%       Types = [].
%       ?- action_types(0'a, Types).
%       Types = [atom].

action_types(0'~, []).
action_types(0'a, [atom]).
action_types(0'c, [integer]).  % specifically, a code
action_types(0'd, [integer]).
action_types(0'D, [integer]).
action_types(0'e, [float]).
action_types(0'E, [float]).
action_types(0'f, [float]).
action_types(0'g, [float]).
action_types(0'G, [float]).
action_types(0'i, [any]).
action_types(0'I, [integer]).
action_types(0'k, [any]).
action_types(0'n, []).
action_types(0'N, []).
action_types(0'p, [any]).
action_types(0'q, [any]).
action_types(0'r, [integer]).
action_types(0'R, [integer]).
action_types(0's, [text]).
action_types(0'@, [callable]).
action_types(0't, []).
action_types(0'|, []).
action_types(0'+, []).
action_types(0'w, [any]).
action_types(0'W, [any, list]).
