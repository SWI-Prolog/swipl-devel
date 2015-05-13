/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2001-2014, University of Amsterdam
			      VU University Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_pretty_print,
	  [ print_term/2	% +Term, +Options
	  ]).
:- use_module(library(option)).

/** <module> Pretty Print Prolog terms

This module is a first  start  of   what  should  become a full-featured
pretty printer for Prolog  terms  with   many  options  and  parameters.
Eventually,  it  should  replace  portray_clause/1   and  various  other
special-purpose predicates.

@tbd This is just a quicky. We  need proper handling of portray/1, avoid
printing very long terms  multiple   times,  spacing (around operators),
etc.

@tbd Use a record for the option-processing.

@tbd The current approach is far too simple, often resulting in illegal
     terms.
*/

:- predicate_options(print_term/2, 2,
		     [ output(stream),
		       right_margin(integer),
		       left_margin(integer),
		       tab_width(integer),
		       indent_arguments(integer),
		       operators(boolean),
		       write_options(list)
		     ]).

%%	print_term(+Term, +Options) is det.
%
%	Pretty print a Prolog term. The following options are processed:
%
%	  * output(+Stream)
%	  Define the output stream.  Default is =user_output=
%	  * right_margin(+Integer)
%	  Width of a line.  Default is 72 characters.
%	  * left_margin(+Integer)
%	  Left margin for continuation lines.  Default is 0.
%	  * tab_width(+Integer)
%	  Distance between tab-stops.  Default is 8 characters.
%	  * indent_arguments(+Spec)
%	  Defines how arguments of compound terms are placed.  Defined
%	  values are:
%	    $ =false= :
%	    Simply place them left to right (no line-breaks)
%	    $ =true= :
%	    Place them vertically, aligned with the open bracket (not
%	    implemented)
%	    $ =auto= (default) :
%	    As horizontal if line-width is not exceeded, vertical
%	    otherwise.
%	    $ An integer :
%	    Place them vertically aligned, <N> spaces to the right of
%	    the beginning of the head.
%	  * operators(+Boolean)
%	  This is the inverse of the write_term/3 option =ignore_ops=.
%	  Default is to respect them.
%	  * write_options(+List)
%	  List of options passed to write_term/3 for terms that are
%	  not further processed.  Default:
%	    ==
%		[ numbervars(true),
%		  quoted(true),
%		  portray(true)
%	        ]
%	    ==

print_term(Term, Options) :-
	\+ \+ print_term_2(Term, Options).

print_term_2(Term, Options0) :-
	prepare_term(Term, Template, Cycles, Constraints),
	defaults(Defs),
	merge_options(Options0, Defs, Options),
	option(write_options(WrtOpts), Options),
	option(max_depth(MaxDepth), WrtOpts, infinite),
	option(left_margin(LeftMargin), Options, 0),
	Context	= ctx(LeftMargin,0,1200,MaxDepth),
	pp(Template, Context, Options),
	print_extra(Cycles, Context, 'where', Options),
	print_extra(Constraints, Context, 'with constraints', Options).

print_extra([], _, _, _) :- !.
print_extra(List, Context, Comment, Options) :-
	option(output(Out), Options),
	format(Out, ', % ~w', [Comment]),
	modify_context(Context, [indent=4], Context1),
	print_extra_2(List, Context1, Options).

print_extra_2([H|T], Context, Options) :-
	option(output(Out), Options),
	context(Context, indent, Indent),
	indent(Out, Indent, Options),
	pp(H, Context, Options),
	(   T == []
	->  true
	;   format(Out, ',', []),
	    print_extra_2(T, Context, Options)
	).


%%	prepare_term(+Term, -Template, -Cycles, -Constraints)
%
%	Prepare a term, possibly  holding   cycles  and  constraints for
%	printing.

prepare_term(Term, Template, Cycles, Constraints) :-
	term_attvars(Term, []), !,
	Constraints = [],
	'$factorize_term'(Term, Template, Factors),
	bind_non_cycles(Factors, 1, Cycles),
	numbervars(Template+Cycles+Constraints, 0, _,
		   [singletons(true)]).
prepare_term(Term, Template, Cycles, Constraints) :-
	copy_term(Term, Copy, Constraints), !,
	'$factorize_term'(Copy, Template, Factors),
	bind_non_cycles(Factors, 1, Cycles),
	numbervars(Template+Cycles+Constraints, 0, _,
		   [singletons(true)]).


bind_non_cycles([], _, []).
bind_non_cycles([V=Term|T], I, L) :-
	unify_with_occurs_check(V, Term), !,
	bind_non_cycles(T, I, L).
bind_non_cycles([H|T0], I, [H|T]) :-
	H = ('$VAR'(Name)=_),
	atom_concat('_S', I, Name),
	I2 is I + 1,
	bind_non_cycles(T0, I2, T).


defaults([ output(user_output),
	   right_margin(72),
	   indent_arguments(auto),
	   operators(true),
	   write_options([ quoted(true),
			   numbervars(true),
			   portray(true),
			   attributes(portray)
			 ])
	 ]).


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
	context(Ctx, max_depth, infinite), !.
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
pp(Portray, _Ctx, Options) :-
	option(write_options(WriteOptions), Options),
	option(portray(true), WriteOptions),
	option(output(Out), Options),
	with_output_to(Out, user:portray(Portray)), !.
pp(List, Ctx, Options) :-
	List = [_|_], !,
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	(   (   IndentStyle == false
	    ->	true
	    ;	IndentStyle == auto,
		print_width(List, Width, Options),
		option(right_margin(RM), Options),
		Indent + Width < RM
	    )
	->  pprint(List, Ctx, Options)
	;   format(Out, '[ ', []),
	    Nindent is Indent + 2,
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx),
	    pp_list_elements(List, NCtx, Options),
	    indent(Out, Indent, Options),
	    format(Out, ']', [])
	).
:- if(current_predicate(is_dict/1)).
pp(Dict, Ctx, Options) :-
	is_dict(Dict), !,
	dict_pairs(Dict, Tag, Pairs),
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	context(Ctx, indent, Indent),
	(   IndentStyle == false ; Pairs == []
	->  pprint(Dict, Ctx, Options)
	;   IndentStyle == auto,
	    print_width(Dict, Width, Options),
	    option(right_margin(RM), Options),
	    Indent + Width < RM		% fits on a line, simply write
	->  pprint(Dict, Ctx, Options)
	;   format(atom(Buf2), '~q{ ', [Tag]),
	    write(Out, Buf2),
	    atom_length(Buf2, FunctorIndent),
	    (   integer(IndentStyle)
	    ->	Nindent is Indent + IndentStyle,
	        (   FunctorIndent > IndentStyle
		->  indent(Out, Nindent, Options)
		;   true
		)
	    ;   Nindent is Indent + FunctorIndent
	    ),
	    context(Ctx, depth, Depth),
	    NDepth is Depth + 1,
	    modify_context(Ctx, [indent=Nindent, depth=NDepth], NCtx0),
	    dec_depth(NCtx0, NCtx),
	    pp_dict_args(Pairs, NCtx, Options),
	    BraceIndent is Nindent - 2,		% '{ '
	    indent(Out, BraceIndent, Options),
	    write(Out, '}')
	).
:- endif.
pp(Term, Ctx, Options) :-		% handle operators
	functor(Term, Name, Arity),
	current_op(Prec, Type, Name),
	match_op(Type, Arity, Kind, Prec, Left, Right),
	option(operators(true), Options), !,
	option(output(Out), Options),
	context(Ctx, indent, Indent),
	context(Ctx, depth, Depth),
	context(Ctx, precedence, CPrec),
	NDepth is Depth + 1,
	modify_context(Ctx, [depth=NDepth], Ctx1),
	dec_depth(Ctx1, Ctx2),
	(   Kind == prefix
	->  arg(1, Term, Arg),
	    (   CPrec >= Prec
	    ->	format(atom(Buf), '~q ', Name),
		atom_length(Buf, AL),
		NIndent is Indent + AL,
		write(Out, Buf),
		modify_context(Ctx2, [indent=NIndent, precedence=Right], Ctx3),
		pp(Arg, Ctx3, Options)
	    ;	format(atom(Buf), '(~q ', Name),
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
		pp(Arg2, Ctx4, Options),
		format(Out, ')', [])
	    )
	).
pp(Term, Ctx, Options) :-		% compound
	option(output(Out), Options),
	option(indent_arguments(IndentStyle), Options),
	context(Ctx, indent, Indent),
	(   IndentStyle == false
	->  pprint(Term, Ctx, Options)
	;   IndentStyle == auto,
	    print_width(Term, Width, Options),
	    option(right_margin(RM), Options),
	    Indent + Width < RM		% fits on a line, simply write
	->  pprint(Term, Ctx, Options)
	;   Term =.. [Name|Args],
	    format(atom(Buf2), '~q(', [Name]),
	    write(Out, Buf2),
	    atom_length(Buf2, FunctorIndent),
	    (   integer(IndentStyle)
	    ->	Nindent is Indent + IndentStyle,
	        (   FunctorIndent > IndentStyle
		->  indent(Out, Nindent, Options)
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
	option(output(Out), Options),
	write(Out, '...').
pp_list_elements([H|T], Ctx0, Options) :-
	dec_depth(Ctx0, Ctx),
	pp(H, Ctx, Options),
	(   T == []
	->  true
	;   nonvar(T),
	    T = [_|_]
	->  option(output(Out), Options),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent, Options),
	    pp_list_elements(T, Ctx, Options)
	;   option(output(Out), Options),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2, Options),
	    write(Out, '| '),
	    pp(T, Ctx, Options)
	).


pp_compound_args([H|T], Ctx, Options) :-
	pp(H, Ctx, Options),
	(   T == []
	->  true
	;   T = [_|_]
	->  option(output(Out), Options),
	    write(Out, ','),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent, Options),
	    pp_compound_args(T, Ctx, Options)
	;   option(output(Out), Options),
	    context(Ctx, indent, Indent),
	    indent(Out, Indent-2, Options),
	    write(Out, '| '),
	    pp(T, Ctx, Options)
	).


:- if(current_predicate(is_dict/1)).
pp_dict_args([Name-Value|T], Ctx, Options) :-
	option(output(Out), Options),
	line_position(Out, Pos0),
	pp(Name, Ctx, Options),
	write(Out, ':'),
	line_position(Out, Pos1),
	context(Ctx, indent, Indent),
	Indent2 is Indent + Pos1-Pos0,
	modify_context(Ctx, [indent=Indent2], Ctx2),
	pp(Value, Ctx2, Options),
	(   T == []
	->  true
	;   option(output(Out), Options),
	    write(Out, ','),
	    indent(Out, Indent, Options),
	    pp_dict_args(T, Ctx, Options)
	).
:- endif.

%	match_op(+Type, +Arity, +Precedence, -LeftPrec, -RightPrec

match_op(fx,	1, prefix,  P, _, R) :- R is P - 1.
match_op(fy,	1, prefix,  P, _, P).
match_op(xf,	1, postfix, P, _, L) :- L is P - 1.
match_op(yf,	1, postfix, P, P, _).
match_op(xfx,	2, infix,   P, A, A) :- A is P - 1.
match_op(xfy,	2, infix,   P, L, P) :- L is P - 1.
match_op(yfx,	2, infix,   P, P, R) :- R is P - 1.


%%	indent(+Out, +Indent, +Options)
%
%	Newline and indent to the indicated  column. Respects the option
%	=tab_width=.  Default  is  8.  If  the  tab-width  equals  zero,
%	indentation is emitted using spaces.

indent(Out, Indent, Options) :-
	option(tab_width(TW), Options, 8),
	nl(Out),
	(   TW =:= 0
	->  tab(Out, Indent)
	;   Tabs is Indent // TW,
	    Spaces is Indent mod TW,
	    forall(between(1, Tabs, _), put(Out, 9)),
	    tab(Out, Spaces)
	).

%%	print_width(+Term, -W, +Options) is det.
%
%	Width required when printing `normally' left-to-right.

print_width(Term, W, Options) :-
	option(right_margin(RM), Options),
	(   write_length(Term, W, [max_length(RM)|Options])
	->  true
	;   W = RM
	).

%%	pprint(+Term, +Context, +Options)
%
%	The bottom-line print-routine.

pprint(Term, Ctx, Options) :-
	option(output(Out), Options),
	pprint(Out, Term, Ctx, Options).

pprint(Out, Term, Ctx, Options) :-
	option(write_options(WriteOptions), Options),
	context(Ctx, max_depth, MaxDepth),
	(   MaxDepth == infinite
	->  write_term(Out, Term, WriteOptions)
	;   MaxDepth =< 0
	->  format(Out, '...', [])
	;   write_term(Out, Term, [max_depth(MaxDepth)|WriteOptions])
	).
