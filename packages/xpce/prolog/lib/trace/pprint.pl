/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(prolog_pretty_print,
	  [ print_term/2	% +Term, +Options
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module is a first  start  of   what  should  become a full-featured
pretty printer for Prolog  terms  with   many  options  and  parameters.
Eventually,  it  should  replace  portray_clause/1   and  various  other
special-purpose predicates.

This is just a quicky.  We  need   proper  handling  of portray/1, avoid
printing very long terms  multiple   times,  spacing (around operators),
etc.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Options:

	Name			Type				Default
	===================================================================
	output			Stream				user_output
	right_margin		Integer				72
	indent_arguments	{auto,true,false}|Integer	auto
	operators,		{true,false},			true
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_term(Term, Options0) :-
	defaults(Defs),
	append(Options0, Defs, Options),
	pp(Term, ctx(0,0,1200), Options).

defaults([ output(user_output),
	   right_margin(72),
	   indent_arguments(2),
	   operators(true)
	 ]).

		 /*******************************
		 *	       OPTIONS		*
		 *******************************/

option(Options, A) :-
	memberchk(A, Options).


		 /*******************************
		 *	       CONTEXT		*
		 *******************************/

context_attribute(indent,     1).
context_attribute(depth,      2).
context_attribute(precedence, 3).

context(Ctx, Name, Value) :-
	context_attribute(Name, Arg),
	arg(Arg, Ctx, Value).

modify_context(Ctx0, Mapping, Ctx) :-
	functor(Ctx0, Name, Arity),
	functor(Ctx,  Name, Arity),
	modify_context(0, Arity, Ctx0, Mapping, Ctx).

modify_context(Arity, Arity, _, _, _) :- !.
modify_context(I, Arity, Ctx0, Mapping, Ctx) :-
	N is I + 1,
	(   context_attribute(Name, N),
	    memberchk(Name=Value, Mapping)
	->  true
	;   arg(N, Ctx0, Value)
	),
	arg(N, Ctx, Value),
	modify_context(N, Arity, Ctx0, Mapping, Ctx).


		 /*******************************
		 *	        PP		*
		 *******************************/

pp(Atom, _Ctx, Options) :-
	atomic(Atom), !,
	option(Options, output(Out)),
	print(Out, Atom).
pp(Var, _Ctx, Options) :-
	var(Var), !,
	option(Options, output(Out)),
	print(Out, Var).
pp(List, Ctx, Options) :-
	List = [_|_], !,
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	option(Options, output(Out)),
	option(Options, indent_arguments(IndentStyle)),
	(   IndentStyle == false
	->  sformat(Out, '~p', [List])
	;   IndentStyle == auto,
	    sformat(Buf, '~p', [List]),
	    atom_length(Buf, AL),
	    option(Options, right_margin(RM)),
	    Indent + AL < RM		% fits on a line, simply write
	->  write(Out, Buf)
	;   format(Out, '[ ', []),
	    Nindent is Indent + 2,
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx),
	    pp_list_elements(List, NCtx, Options),
	    indent(Out, Indent),
	    format(Out, ']', [])
	).
pp(Term, Ctx, Options) :-		% handle operators
	functor(Term, Name, Arity),
	current_op(Prec, Type, Name),
	match_op(Type, Arity, Kind, Prec, Left, Right),
	option(Options, operators(true)), !,
	option(Options, output(Out)),
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	context(Ctx, precedence, CPrec),
	NDepth is Depth + 1,
	modify_context(Ctx, [depth=NDepth], Ctx2),
	(   Kind == prefix
	->  arg(1, Term, Arg),
	    (   CPrec >= Prec
	    ->	sformat(Buf, '~q ', Name),
		atom_length(Buf, AL),
		NIndent is Indent + AL,
		write(Out, Buf),
		modify_context(Ctx2, [indent=NIndent, precedence=Right], Ctx3),
		pp(Arg, Ctx3, Options)
	    ;	sformat(Buf, '(~q ', Name),
		atom_length(Buf, AL),
		NIndent is Indent + AL,
		write(Out, Buf),
		modify_context(Ctx2, [indent=NIndent, precedence=Right], Ctx3),
		pp(Arg, Ctx3, Options),
		format(Out, ')', [])
	    )
	;   Kind == postfix
	->  arg(1, Term, Arg),
	    (   CPrec >= Prec
	    ->  modify_context(Ctx2, [precedence=Left], Ctx3),
	        pp(Arg, Ctx3, Options),
		format(Out, ' ~q', Name)
	    ;	format(Out, '(', []),
		NIndent is Indent + 1,
		modify_context(Ctx2, [indent=NIndent, precedence=Left], Ctx3),
		pp(Arg, Ctx3, Options),
		format(Out, ' ~q)', [Name])
	    )
	;   arg(1, Term, Arg1),
	    arg(2, Term, Arg2),
	    (	CPrec >= Prec
	    ->  modify_context(Ctx2, [precedence=Left], Ctx3),
		pp(Arg1, Ctx3, Options),
		format(Out, ' ~q ', Name),
		modify_context(Ctx2, [precedence=Right], Ctx4),
		pp(Arg2, Ctx4, Options)
	    ;	format(Out, '(', []),
		NIndent is Indent + 1,
		modify_context(Ctx2, [indent=NIndent, precedence=Left], Ctx3),
		pp(Arg1, Ctx3, Options),
		format(Out, ' ~q ', Name),
		modify_context(Ctx2, [precedence=Right], Ctx4),
		pp(Arg2, Ctx2, Options),
		format(Out, ')', [])
	    )
	).
pp(Term, Ctx, Options) :-
	option(Options, output(Out)),
	option(Options, indent_arguments(IndentStyle)),
	context(Ctx, indent, Indent),
	(   IndentStyle == false
	->  sformat(Out, '~p', [Term])
	;   IndentStyle == auto,
	    sformat(Buf, '~p', [Term]),
	    atom_length(Buf, AL),
	    option(Options, right_margin(RM)),
	    Indent + AL < RM		% fits on a line, simply write
	->  write(Out, Buf)
	;   Term =.. [Name|Args],
	    sformat(Buf2, '~q(', [Name]),
	    write(Out, Buf2),
	    atom_length(Buf2, FunctorIndent),
	    (   integer(IndentStyle)
	    ->	Nindent is Indent + IndentStyle,
	        (   FunctorIndent > IndentStyle
		->  indent(Out, Nindent)
		;   true
		)
	    ;   Nindent is Indent + FunctorIndent
	    ),
	    context(Ctx, depth, Depth),
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx),
	    pp_list_elements(Args, NCtx, Options),
	    write(Out, ')')
	).
	    

pp_list_elements([X|T], Ctx, Options) :-
	\+ is_list(T), !,
	pp(X, Ctx, Options),
	option(Options, output(Out)),
	context(Ctx, indent, Indent),
	indent(Out, Indent-2),
	write(Out, '| '),
	pp(T, Ctx, Options).
pp_list_elements([X], Ctx, Options) :- !,
	pp(X, Ctx, Options).
pp_list_elements([H|T], Ctx, Options) :-
	pp(H, Ctx, Options),
	option(Options, output(Out)),
	write(Out, ','),
	context(Ctx, indent, Indent),
	indent(Out, Indent),
	pp_list_elements(T, Ctx, Options).

%	match_op(+Type, +Arity, +Precedence, -LeftPrecedence, -RightPrecedence

match_op(fx,	1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,	1, prefix,  P, _, P).
match_op(xf,	1, postfix, P, _, L) :- L is P - 1.
match_op(yf,	1, postfix, P, P, _).
match_op(xfx,	2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,	2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,	2, infix,   P, P, R) :- R is P - 1.


indent(Out, Indent) :-
	nl(Out),
	Tabs is Indent // 8,
	Spaces is Indent mod 8,
	forall(between(1, Tabs, _), put(Out, 9)),
	tab(Out, Spaces).
	

	    
	
