/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(am_match,
	[ am_match/1	% Does Atom matches previous compiled expression?
	, am_compile/1	% Compile expression represented by atomic argument
	, am_bagof/4	% Return alternatives of Var regarding
	]).		% Goal and matching Expr.


/** <module> Simple regex library

@bug	Old stuff.  Currently the only serious implementation of
	regular expression in SWI-Prolog is provided by XPCE.
*/

am_bagof(Var, Expression, Goal, Bag) :-
	am_compile(Expression), !,
	bagof(Var, am_bagof_goal(Var, Goal), Bag), !.

am_bagof_goal(Var, Goal) :-
	Goal,
	am_match(Var).

am_match(Atom) :-
	name(Atom, String),
	recorded(am_compiled, goal(Goal, String), _), !,
	Goal, !.

am_compile(Reg) :-
	am_compile(Reg, Goal, String),
	(   recorded(am_compiled, goal(_, _), Ref)
	->  erase(Ref)
	;   true
	),
	recorda(am_compiled, goal(Goal, String), _), !.

am_compile(Reg, Goal, String) :-
	name(Reg, RegString),
	am_token_list(Tokens, RegString, ""),
	am_comp(Tokens, Goal, String), !.

am_comp([], (Full = ""), Full) :- !.
am_comp([string(String)|Tokens],
	 (append(String, Rest, Full), Goal), Full) :- !,
	am_comp(Tokens, Goal, Rest).
am_comp([star|Tokens],
	 (append(_, Rest, Full), Goal), Full) :- !,
	am_comp(Tokens, Goal, Rest).
am_comp([any|Tokens],
	 (append([_], Rest, Full), Goal), Full) :- !,
	am_comp(Tokens, Goal, Rest).
am_comp([anychar(Chars)|Tokens],
	 (append([C], Rest, Full), member(C, Chars), Goal), Full) :- !,
	am_comp(Tokens, Goal, Rest).
am_comp([anyof(Strings)|Tokens],
	 (OrGoal, Goal), Full) :- !,
	am_anyof_goal(Strings, Rest, Full, OrGoal),
	am_comp(Tokens, Goal, Rest).

am_anyof_goal([One], Rest, Full, append(One, Rest, Full) ) :- !.
am_anyof_goal([Head|Tail], Rest, Full, (append(Head, Rest, Full) | Goal)) :-
	am_anyof_goal(Tail, Rest, Full, Goal).

am_token_list([]) --> !.
am_token_list([Token|TokenList]) -->
	am_token(Token),
	am_token_list(TokenList).

am_token(anyof([String|Rest])) -->
	am_next_char("{"),
	am_any_string(String),
	am_any_strings(Rest),
	am_next_char("}"), !.
am_token(anychar(List) ) -->
	am_next_char("["),
	am_anychar(List),
	am_next_char("]"), !.
am_token(star) -->
	am_next_char("*"), !.
am_token(any) -->
	am_next_char("?"), !.
am_token(string(String) ) -->
	am_max_string(String), !.

am_any_strings([String|Rest]) -->
	am_next_char(", "),
	am_any_string(String),
	am_any_strings(Rest).
am_any_strings([]) --> { true }.

am_anychar([]) -->
	am_see_char("]"), !.
am_anychar(List) -->
	am_next_char([C1]),
	am_next_char("-"),
	am_next_char([C2]), { C2 \== 93, C2 > C1 },
	{ am_anylist(C1, C2, Sofar) },
	am_anychar(Rest),
	{ append(Sofar, Rest, List) }, !.
am_anychar([C|Rest]) -->
	am_next_char([C]), !,
	am_anychar(Rest).

am_max_string([C|Rest]) -->
	am_next_char("\\"),
	am_next_char([C]), !,
	am_max_string(Rest).
am_max_string([C|Rest]) -->
	am_next_char([C]),
	{  \+ member(C, "[]{}?*") }, !,
	am_max_string(Rest).
am_max_string([]) --> { true }.

am_any_string([]) -->
	am_see_char("}"), !.
am_any_string([]) -->
	am_see_char(", "), !.
am_any_string([C|Rest]) -->
	am_next_char("\\"),
	am_next_char([C]), !,
	am_any_string(Rest).
am_any_string([C|Rest]) -->
	am_next_char([C]), !,
	am_any_string(Rest).

am_next_char([C], [C|R], R).		% read next character
am_see_char(  [C], [C|R], [C|R]).	% line am_next_char, don't read

/*  fill a list with all characters between C1 and C2.
*/

am_anylist(C1, C1, []) :- !.
am_anylist(C1, C2, [C1|Rest]) :-
	succ(C1, CN),
	am_anylist(CN, C2, Rest).
