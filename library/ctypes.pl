/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Character classification
*/

:- module( ctypes,[
	is_alnum/1,
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
	is_quote/1,
	is_space/1,
	is_upper/1,
	is_white/1,
	to_lower/2,
	to_upper/2] ).

%	This file implements the functionality of the corresponding Quintus
%	library based on SWI-Prolog's code_type/2 predicate.  Please check
%	the decumentation of this predicate to find the definitions of the
%	classes.

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
is_newpage(12).				% Control-L
is_newline(10).

is_paren(0'(, 0')).			% Prolog is too good at this
is_paren(0'[, 0']).
is_paren(0'{, 0'}).

%	to_lower( ?U,?L )
%	Succeeds  if  `U'  is  upper  case  character  and  `L'  is  the
%	corresponding lower case character or `U' is an ascii character,
%	but not an upper case letter and `L' is equal to `U'.

to_lower(U, L) :-
	code_type(L, to_lower(U)).

to_upper(U, L) :-
	code_type(L, to_upper(U)).

%	is_digit( ?C,?Base,?Weight )
%	Succeeds if `C' is a digit using `Base'  as  base  and  `Weight'
%	represents its value.  Only the base-10 case is handled by code_type.

is_digit(C, Base, Weight) :-
	Base == 10, !,
	code_type(C, digit(Weight)).
is_digit(C, Base, Weight) :-
	between(2, 36, Base),
	succ(X, Base),
	between(0, X, Weight),
	is_digit(C, Weight).

is_digit(C, Weight) :-
	Weight < 10, !,
	plus(Weight, 0'0, C).
is_digit(C, Weight) :-
	plus(Weight, 87, C), !.		/* `a`-10 */
is_digit(C, Weight) :-
	plus(Weight, 55, C).		/* `A`-10 */

