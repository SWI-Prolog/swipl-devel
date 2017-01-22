/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1985-2012, University of Amsterdam
                              VU University Amsterdam
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

:- module(ctypes,
          [ is_alnum/1,
            is_alpha/1,
            is_ascii/1,
            is_cntrl/1,
            is_csym/1,
            is_csymf/1,
            is_digit/1,
            is_digit/3,
            is_endfile/1,
            is_endline/1,
            is_graph/1,
            is_lower/1,
            is_newline/1,
            is_newpage/1,
            is_paren/2,
            is_period/1,
            is_print/1,
            is_punct/1,
            is_quote/1,
            is_space/1,
            is_upper/1,
            is_white/1,
            to_lower/2,
            to_upper/2,
            upper_lower/2
          ]).

/** <module> Character code classification

This file implements the functionality of the corresponding Quintus
library based on SWI-Prolog's code_type/2 predicate. Please check the
documentation of this predicate to find the definitions of the classes.

@see    code_type/2
@see    char_type/2
*/

is_alnum(C)   :- code_type(C, alnum).
is_alpha(C)   :- code_type(C, alpha).
is_ascii(C)   :- code_type(C, ascii).
is_cntrl(C)   :- code_type(C, cntrl).
is_csym(C)    :- code_type(C, csym).
is_csymf(C)   :- code_type(C, csymf).
is_digit(C)   :- code_type(C, digit).
is_graph(C)   :- code_type(C, graph).
is_lower(C)   :- code_type(C, lower).
is_upper(C)   :- code_type(C, upper).
is_period(C)  :- code_type(C, period).
is_endline(C) :- code_type(C, end_of_line).
is_print(C)   :- is_graph(C).
is_punct(C)   :- code_type(C, punct).
is_quote(C)   :- code_type(C, quote).
is_space(C)   :- code_type(C, space).
is_white(C)   :- code_type(C, white).

is_endfile(-1).
is_newpage(12).                         % Control-L
is_newline(10).

%!  is_paren(?Open, ?Close) is semidet.
%
%   True if Open is the open-parenthesis of Close.

is_paren(0'(, 0')).                     % Prolog is too good at this
is_paren(0'[, 0']).
is_paren(0'{, 0'}).

%!  to_lower(+U, -L) is det.
%
%   Downcase a character code. If U  is   the  character  code of an
%   uppercase character, unify L with  the   character  code  of the
%   lowercase version. Else unify L with U.

to_lower(U, L) :-
    code_type(L, to_lower(U)).

%!  to_upper(+L, -U) is det.
%
%   Upcase a character code.  If  L  is   the  character  code  of a
%   lowercase character, unify L with  the   character  code  of the
%   uppercase version. Else unify U with L.

to_upper(L, U) :-
    code_type(U, to_upper(L)).

%!  upper_lower(?U, ?L) is det.
%
%   True when U is the character code  of an uppercase character and
%   L  is  the  character  code    of  the  corresponding  lowercase
%   character.

upper_lower(Upper, Lower) :-
    nonvar(Upper),
    !,
    code_type(Lower, lower(Upper)).
upper_lower(Upper, Lower) :-
    code_type(Upper, upper(Lower)).


%!  is_digit(+C, +Base, -Weight) is det.
%!  is_digit(-C, +Base, +Weight) is det.
%
%   Succeeds if `C' is a digit using `Base'  as  base  and  `Weight'
%   represents its value.  Only the base-10 case is handled by code_type.

is_digit(C, Base, Weight) :-
    Base == 10,
    !,
    code_type(C, digit(Weight)).
is_digit(C, Base, Weight) :-
    between(2, 36, Base),
    succ(X, Base),
    between(0, X, Weight),
    is_digit(C, Weight).

is_digit(C, Weight) :-
    Weight < 10,
    !,
    plus(Weight, 0'0, C).
is_digit(C, Weight) :-
    plus(Weight, 87, C),
    !.         /* `a`-10 */
is_digit(C, Weight) :-
    plus(Weight, 55, C).            /* `A`-10 */

