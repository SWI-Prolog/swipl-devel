/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2018, University of Amsterdam
                              VU University Amsterdam
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

:- module(atom,
          [ restyle_identifier/3,               % +Style, +In, +Out
            identifier_parts/2,                 % +Identifier, -Parts
            join_identifier_parts/3             % +Style, +Parts, -Identifier
          ]).
:- use_module(library(apply)).

/** <module> Operations on atoms

This library provides operations  on  atoms   that  are  not  covered by
builtin predicates. The current implementation is   just a start, making
code developed in _xpce_ and duplicated in various projects reusable.
*/


                 /*******************************
                 *      RESTYLE IDENTIFIERS     *
                 *******************************/

%!  restyle_identifier(+Style, +In, -Out) is det.
%
%   Restyle an identifier by extracting the alnum substrings and
%   joining them together according to Style.
%
%   @arg Style is one of `'OneTwo'`, `oneTwo`, `one_two`, `'One_Two'` or
%   a term style(CapitaliseFirst, CapitaliseRest, Separator).

%   @see join_identifier_parts/3.

restyle_identifier(Style, In, Out) :-
    identifier_parts(In, Parts),
    join_identifier_parts(Style, Parts, Out).


%!  identifier_parts(+Identifier, -Parts) is det.
%
%   Parts is a list of atoms  that   make  up  Identifier. The parts
%   found are turned into lowercase, unless   all its characters are
%   uppercase.  E.g.,
%
%       ?- identifier_parts('sourceCodeURI', X).
%       X = [source, code, 'URI'].

identifier_parts(';', [';']) :- !.
identifier_parts('|', ['|']) :- !.
identifier_parts('!', ['!']) :- !.
identifier_parts(',', [',']) :- !.
identifier_parts(Name, Parts) :-
    atom_codes(Name, Codes),
    (   phrase(identifier_parts(Parts), Codes)
    ->  true
    ;   maplist(is_symbol_code, Codes)
    ->  Parts = [Name]
    ).

is_symbol_code(Code) :-
    code_type(Code, prolog_symbol).

identifier_parts([H|T]) -->
    identifier_part(H),
    !,
    identifier_parts(T).
identifier_parts([]) --> [].

identifier_part(H) -->
    string(Codes, Tail),
    sep(Tail),
    !,
    { Codes = [_|_],
      atom_codes(H0, Codes),
      (   maplist(is_upper, Codes)
      ->  H = H0
      ;   downcase_atom(H0, H)
      )
    }.

string(T,T) --> [].
string([H|T], L) --> [H], string(T, L).

sep([]) --> sep_char, !, sep_chars.
sep([T]), [N] -->
    [T,N],
    { code_type(T, lower),
      code_type(N, upper)
    }.
sep([],[],[]).

sep_char -->
    [H],
    { \+ code_type(H, alnum) }.

sep_chars --> sep_char, !, sep_chars.
sep_chars --> [].

%!  join_identifier_parts(+Style, +Parts, -Identifier)
%
%   Join parts of an identifier according to Style. Style is one of:
%
%     - 'OneTwo'
%     - oneTwo
%     - one_two
%     - 'One_Two'
%       Predefined self explanatory style identifiers
%     - style(+CapitaliseFirst, +CapitaliseRest, +Separator)
%       This generalises the above styles.  CapitaliseFirst defines
%       whether the first character of the first part must be a
%       capital, CapitaliseRest defines capitalization of the remaining
%       identifier parts and Separator is the character to place between
%       the parts.

join_identifier_parts(Style, [First|Parts], Identifier) :-
    style(Style, CapFirst, CapRest, Sep),
    capitalise(CapFirst, First, H),
    maplist(capitalise(CapRest), Parts, T),
    atomic_list_concat([H|T], Sep, Identifier).

%!  style(?Style, ?CapitaliseFirst, ?CapitaliseRest, ?Separator)

style('OneTwo',  true,  true,  '').
style(oneTwo,    false, true,  '').
style(one_two,   false, false, '_').
style('One_Two', true,  true,  '_').
style(style(CFirst, CRest, Sep), CFirst, CRest, Sep).

capitalise(false, X, X) :- !.
capitalise(true, X, Y) :-
    atom_codes(X, [H0|T]),
    code_type(H0, to_lower(H)),
    atom_codes(Y, [H|T]).


