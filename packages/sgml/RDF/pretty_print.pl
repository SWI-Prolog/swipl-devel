/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

:- module(dia_pretty_print,
	  [ pretty_print/1
	  ]).

:- require([ atom_length/2
	   , between/3
	   , forall/2
	   , is_list/1
	   , member/2
	   , memberchk/2
	   ]).


pretty_print(Term) :-
	numbervars(Term, 0, _),
	pp(Term, 0),
	write('.'), nl, fail.
pretty_print(_).


pp(Term, _Indent) :-
	atomic(Term), !,
	writeq(Term).
pp(Var, _Indent) :-
	var(Var), !,
	write(Var).
pp(Var, _Indent) :-
	Var = '$VAR'(_), !,
	print(Var).
pp('$aref'(Name), _Indent) :- !,
	write(Name).
pp(Module:Term, Indent) :-
	atomic(Module), !,
	writeq(Module), write(:),
	pp(Term, Indent).
pp([A1 := V1|ArgList], Indent) :-	% [] is done by `atomic'!
	is_list(ArgList),
	forall(member(A, ArgList), A = (_ := _)), !,
	longest_attribute([A1 := V1|ArgList], 0, L),
	NewIndent is Indent + 2,
	(   L > 9, Indent < 25, length(ArgList, Args), Args > 1
	->  ArgIndent is Indent + 4,
	    ValGoal = (nl, indent(ArgIndent))
	;   ArgIndent is Indent + 6 + L,
	    ValGoal = write(' ')
	),
	write('[ '),
	pp(A1, Indent), term_length(A1, L1),
	tab(L-L1), write(' :='), ValGoal,
	pp(V1, ArgIndent),
	forall(member(A := V, ArgList),
	       (write(','), nl,
		indent(NewIndent),
		pp(A, Indent), term_length(A, LA), tab(L-LA),
		write(' :='), ValGoal, pp(V, ArgIndent))),
	nl,
	indent(Indent),
	write(']').
pp([H|T], Indent) :-
	is_list(T), !,
	write('[ '),
	NewIndent is Indent + 2,
	pp(H, NewIndent),
	forall(member(E, T),
	       (write(','), nl,
		indent(NewIndent),
		pp(E, NewIndent))),
	nl,
	indent(Indent),
	write(']').
pp(Term, Indent) :-
	functor(Term, Name, 2),
	current_op(_, Type, Name),
	memberchk(Type, [xfx, yfx]), !,
	arg(1, Term, A1),
	arg(2, Term, A2),
	pp(A1, Indent), format(' ~q ', [Name]), pp(A2, Indent).
pp(Term, Indent) :-
	functor(Term, Name, _Arity),
	atom_length(Name, L),
	NewIndent is Indent + L + 1,
	format('~q(', Name),
	(   term_argument_length(Term, AL),
	    NewIndent + AL < 72
	->  Wrap = nowrap
	;   Wrap = wrap
	),
	forall(generate_arg(I, Term, Arg),
	       pparg(I, Arg, Wrap, NewIndent)),
	write(')').

generate_arg(ArgN, Term, Arg) :-
	functor(Term, _, Arity),
	between(1, Arity, ArgN),
	arg(ArgN, Term, Arg).

pparg(1, Term, _, Indent) :- !,
	pp(Term, Indent).
pparg(_, Term, wrap, Indent) :- !,
	write(','), nl,
	indent(Indent),
	pp(Term, Indent).
pparg(_, Term, _, Indent) :-
	write(', '),
	pp(Term, Indent).

longest_attribute([], L, L).
longest_attribute([A := _|T], L0, L) :-
	term_length(A, AL),
	max(L0, AL, L1),
	longest_attribute(T, L1, L).

term_length(A, AL) :-
	atomic(A), !,
	atom_length(A, AL).
term_length(Var, AL) :-
	var(Var), !,
	AL = 1.
term_length('$VAR'(N), AL) :-
	varname(N, L),
	length(L, AL).
term_length('$aref'(N), AL) :-
	atom_length(N, AL).

term_argument_length(Term, L) :-
	term_argument_length(Term, 1, 0, L).

term_argument_length(Term, A, L0, L) :-
	arg(A, Term, Arg), !,
	term_length(Arg, AL),
	L1 is AL + L0,
	NA is A + 1,
	term_argument_length(Term, NA, L1, L).
term_argument_length(_, _, L, L).


max(A, B, M) :-
	(   A >= B
	->  M = A
	;   M = B
	).


varname(N, [C]) :-
	N < 26, !, 
	C is N + 0'A.
varname(N, [C1, C2]) :-
	C1 is N // 26 + 0'A, 
	C2 is N mod 26 + 0'A.

indent(I) :-
	Tabs is I // 8,
	forall(between(1, Tabs, _), put(9)),
	Spaces is I mod 8,
	tab(Spaces).
