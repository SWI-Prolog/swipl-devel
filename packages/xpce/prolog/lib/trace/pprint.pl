/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Version History:
    
    2003/07/14   fixed pp_list_elements/3 so it can handle open-ended lists
                          David Reitter, reitter at mle.media.mit.edu
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


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
	output			Stream			   user_output
	right_margin		Integer			   72
	indent_arguments	{auto,true,false}|Integer  auto
	operators,		{true,false},		   true
	write_options,		Option List		 [ numbervars(true),
							   quoted(true),
							   portray(true)
							 ]
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

print_term(Term, Options0) :-
	defaults(Defs),
	append(Options0, Defs, Options1),
	select(write_options(WrtOpts0), Options1, Options2), !,
	(   select(max_depth(MaxDepth), WrtOpts0, WrtOpts)
	->  Options = [write_options(WrtOpts)|Options2]
	;   MaxDepth = inf,
	    Options = Options1
	),
	pp(Term, ctx(0,0,1200,MaxDepth), Options).

defaults([ output(user_output),
	   right_margin(72),
	   indent_arguments(auto),
	   operators(true),
	   write_options([ quoted(true),
			   numbervars(true),
			   portray(true)
			 ])
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
context_attribute(max_depth,  4).

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


dec_depth(Ctx, Ctx) :-
	context(Ctx, max_depth, inf), !.
dec_depth(ctx(I,D,P,MD0), ctx(I,D,P,MD)) :-
	MD is MD0 - 1.
	

		 /*******************************
		 *	        PP		*
		 *******************************/

pp(Primitive, Ctx, Options) :-
	(   atomic(Primitive)
	;   var(Primitive)
	), !,
	pprint(Primitive, Ctx, Options).
pp(List, Ctx, Options) :-
	List = [_|_], !,
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	option(Options, output(Out)),
	option(Options, indent_arguments(IndentStyle)),
	(   (   IndentStyle == false
	    ->	true
	    ;	IndentStyle == auto,
		print_width(List, Ctx, Options, Width),
		option(Options, right_margin(RM)),
		Indent + Width < RM
	    )
	->  pprint(List, Ctx, Options)
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
	modify_context(Ctx, [depth=NDepth], Ctx1),
	dec_depth(Ctx1, Ctx2),
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
pp(Term, Ctx, Options) :-		% compound
	option(Options, output(Out)),
	option(Options, indent_arguments(IndentStyle)),
	context(Ctx, indent, Indent),
	(   IndentStyle == false
	->  pprint(Term, Ctx, Options)
	;   IndentStyle == auto,
	    print_width(Term, Ctx, Options, Width),
	    option(Options, right_margin(RM)),
	    Indent + Width < RM		% fits on a line, simply write
	->  pprint(Term, Ctx, Options)
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
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
	    dec_depth(NCtx0, NCtx),
	    pp_compound_args(Args, NCtx, Options),
	    write(Out, ')')
	).
	    

pp_list_elements(_, Ctx, Options) :-
	context(Ctx, max_depth, 0), !,
	option(Options, output(Out)),
	write(Out, '...').
pp_list_elements([H|T], Ctx0, Options) :-
	dec_depth(Ctx0, Ctx),
	pp(H, Ctx, Options),
	(   T == []
	->  true
	;   nonvar(T),
	    T = [_|_]
	->  option(Options, output(Out)),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent),
	    pp_list_elements(T, Ctx, Options)
	;   option(Options, output(Out)),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2),
	    write(Out, '| '),
	    pp(T, Ctx, Options)
	).


pp_compound_args([H|T], Ctx, Options) :-
	pp(H, Ctx, Options),
	(   T == []
	->  true
	;   T = [_|_]
	->  option(Options, output(Out)),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent),
	    pp_compound_args(T, Ctx, Options)
	;   option(Options, output(Out)),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2),
	    write(Out, '| '),
	    pp(T, Ctx, Options)
	).


%	match_op(+Type, +Arity, +Precedence, -LeftPrec, -RightPrec

match_op(fx,	1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,	1, prefix,  P, _, P).
match_op(xf,	1, postfix, P, _, L) :- L is P - 1.
match_op(yf,	1, postfix, P, P, _).
match_op(xfx,	2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,	2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,	2, infix,   P, P, R) :- R is P - 1.


%	indent(+Out, +Indent)
%	
%	Newline and indent to the indicated column.

indent(Out, Indent) :-
	nl(Out),
	Tabs is Indent // 8,
	Spaces is Indent mod 8,
	forall(between(1, Tabs, _), put(Out, 9)),
	tab(Out, Spaces).
	

%	print_width(+Term, +Context, +Options, -W)
%	
%	Width required when printing `normally' left-to-right.

print_width(Term, Ctx, Options, W) :-
	open_null_stream(Out),
	pprint(Out, Term, Ctx, Options),
%	line_position(Out, W),
	character_count(Out, W),	
	close(Out).


%	pprint(+Term, +Context, +Options)
%	
%	The bottom-line print-routine.

pprint(Term, Ctx, Options) :-
	option(Options, output(Out)),
	pprint(Out, Term, Ctx, Options).

pprint(Out, Term, Ctx, Options) :-
	option(Options, write_options(WriteOptions)),
	context(Ctx, max_depth, MaxDepth),
	(   MaxDepth == inf
	->  write_term(Out, Term, WriteOptions)
	;   MaxDepth =< 0
	->  format(Out, '...', [])
	;   write_term(Out, Term, [max_depth(MaxDepth)|WriteOptions])
	).
