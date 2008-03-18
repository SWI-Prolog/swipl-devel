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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(prolog_listing,
	[ listing/0,
	  listing/1,
	  portray_clause/1,		% +Clause
	  portray_clause/2		% +Stream, +Clause
	]).
:- use_module(library(lists), [member/2]).
%:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
	listing/0, 
	listing/1, 
	list_predicates/2.

:- multifile
	prolog:locate_clauses/2.	% +Spec, -ClauseRefList

%%	listing
%	
%	Lists all predicates defined in the calling module. Imported
%	predicates are not listed.

listing :-
	context_module(Context),
	current_predicate(_, Pred),
	\+ predicate_property(Pred, imported_from(_)),
	strip_module(Pred, Module, Head),
	functor(Head, Name, _Arity),
	(   (   predicate_property(Pred, built_in)
	    ;	sub_atom(Name, 0, _, _, $)
	    )
	->  style_check(?(dollar))
	;   true
	),
	nl, 
	list_predicate(Module:Head, Context),
	fail.
listing.


%%	listing(+PredIndicators)
%	
%	List the given predicates

listing(V) :-
	var(V), !,       % ignore variables
	throw(error(instantiation_error, _)).
listing([]) :- !.
listing([X|Rest]) :- !, 
        listing(X), 
        listing(Rest).
listing(X) :-
	(   prolog:locate_clauses(X, ClauseRefs)
	->  list_clauserefs(ClauseRefs)
	;   '$find_predicate'(X, Preds), 
	    list_predicates(Preds, X)
	).

list_clauserefs([]) :- !.
list_clauserefs([H|T]) :- !,
	list_clauserefs(H),
	list_clauserefs(T).
list_clauserefs(Ref) :-
	clause(Head, Body, Ref),
	portray_clause((Head :- Body)).


list_predicates(Preds, X) :-
	context_module(Context),
	member(Pred, Preds),
	unify_args(Pred, X),
	'$define_predicate'(Pred),
	strip_module(Pred, Module, Head), 
        list_predicate(Module:Head, Context),
	nl,
        fail.
list_predicates(_, _).

%	Unify the arguments of the specification with the given term,
%	so we can partially instantate the head.

unify_args(_, _/_) :- !.		% Name/arity spec
unify_args(X, X) :- !.
unify_args(_:X, X) :- !.
unify_args(_, _).

list_predicate(Pred, Context) :-
	predicate_property(Pred, undefined), !, 
	decl_term(Pred, Context, Decl),
	format('%   Undefined: ~q~n', [Decl]).
list_predicate(Pred, Context) :-
	predicate_property(Pred, foreign), !, 
	decl_term(Pred, Context, Decl),
	format('%   Foreign: ~q~n', [Decl]).
list_predicate(Pred, Context) :-
	notify_changed(Pred, Context),
	list_declarations(Pred, Context), 
	list_clauses(Pred, Context).

decl_term(Pred, Context, Decl) :-
	strip_module(Pred, Module, Head),
	functor(Head, Name, Arity),
	(   (Module == system; Context == Module)
	->  Decl = Name/Arity
	;   Decl = Module:Name/Arity
	).


decl(thread_local, thread_local).
decl(dynamic,	   dynamic).
decl(volatile,	   volatile).
decl(multifile,	   multifile).
decl(transparent,  module_transparent).

declaration(Pred, Source, Decl) :-
	decl(Prop, Declname),
	predicate_property(Pred, Prop),
	decl_term(Pred, Source, Funct),
	Decl =.. [ Declname, Funct ].
	    
list_declarations(Pred, Source) :-
	findall(Decl, declaration(Pred, Source, Decl), Decls),
	(   Decls == []
	->  true
	;   write_declarations(Decls, Source),
	    format('~n', [])
	).


write_declarations([], _) :- !.
write_declarations([H|T], Module) :-
	format(':- ~q.~n', [H]),
	write_declarations(T, Module).

list_clauses(Pred, Source) :-
	strip_module(Pred, Module, Head), 
	(   clause(Pred, Body), 
	    write_module(Module, Source), 
	    portray_clause((Head:-Body)),
	    fail
	;   true
	).

write_module(system, _) :- !.
write_module(Module, Module) :- !.
write_module(Module, _) :-
	format('~q:', [Module]).

notify_changed(Pred, Context) :-
	strip_module(Pred, user, Head),
	predicate_property(Head, built_in),
	\+ predicate_property(Head, (dynamic)), !,
	decl_term(Pred, Context, Decl),
	format('%   NOTE: system definition has been overruled for ~q~n',
	       [Decl]).
notify_changed(_, _).

%%	portray_clause(+Clause) is det.
%%	portray_clause(+Out:stream, +Clause) is det.
%
%	Portray `Clause' on the current  output   stream.  Layout of the
%	clause is to our best standards.   As  the actual variable names
%	are not available we use A, B, ... Deals with ';', '|', '->' and
%	various calls via  meta-call  predicates.   If  Clause  contains
%	attributed variables, these are treated as normal variables.

%	The prolog_list_goal/1 hook is  a  dubious   as  it  may lead to
%	confusion if the heads relates to other   bodies.  For now it is
%	only used for XPCE methods and works just nice.
%	
%	Not really ...  It may confuse the source-level debugger.

%portray_clause(Head :- _Body) :-
%	user:prolog_list_goal(Head), !.
portray_clause(Term) :-
	current_output(Out),
	portray_clause(Out, Term).

portray_clause(Stream, Term) :-
	\+ \+ ( copy_term_nat(Term, Copy),
		numbervars(Copy, 0, _,
			   [ singletons(true)
			   ]), 
		do_portray_clause(Stream, Copy)
	      ).

do_portray_clause(Out, Var) :-
	var(Var), !,
	pprint(Out, Var, 1200).
do_portray_clause(Out, (Head :- true)) :- !, 
	pprint(Out, Head, 1200),
	full_stop(Out).
do_portray_clause(Out, (Head :- Body)) :- !, 
	inc_indent(0, 1, Indent),
	infix_op((:-), LeftPri, RightPri),
	pprint(Out, Head, LeftPri),
	write(Out, ' :-'), 
	(   nonvar(Body),
	    Body = Module:LocalBody
	->  nlindent(Out, Indent),
	    format(Out, '~q', [Module]),
	    '$put_token'(Out, :),
	    nlindent(Out, Indent),
	    write(Out, '(   '),
	    portray_body(LocalBody, 2, noindent, 1200, Out),
	    nlindent(Out, Indent),
	    write(Out, ')')
	;   inc_indent(0, 2, BodyIndent),
	    portray_body(Body, BodyIndent, indent, RightPri, Out)
	),
	full_stop(Out).
do_portray_clause(Out, (:-use_module(File, Imports))) :-
	length(Imports, Len),
	Len > 3, !,
	format(Out, ':- use_module(~q,', [File]),
	portray_list(Imports, 14, Out),
	write(Out, ').\n').
do_portray_clause(Out, (:-module(Module, Exports))) :- !,
	format(Out, ':- module(~q,', [Module]),
	portray_list(Exports, 10, Out),
	write(Out, ').\n').
do_portray_clause(Out, (:-Directive)) :- !,
	write(Out, ':- '), 
	portray_body(Directive, 3, noindent, 1199, Out),
	full_stop(Out).
do_portray_clause(Out, Fact) :-
	portray_body(Fact, 0, noindent, 1200, Out),
	full_stop(Out).

full_stop(Out) :-
	'$put_token'(Out, '.'),
	nl(Out).


%%	portray_body(+Term, +Indent, +DoIndent, +Priority, +Out)
%	
%	Write Term at current indentation. If   DoIndent  is 'indent' we
%	must first call nlindent/2 before emitting anything.

portray_body(Var, _, _, Pri, Out) :-
	var(Var), !,
	pprint(Out, Var, Pri).
portray_body(!, _, _, _, Out) :- !, 
	write(Out, ' !').
portray_body((!, Clause), Indent, _, Pri, Out) :-
	\+ term_needs_braces((_,_), Pri), !,
	write(Out, ' !,'), 
	portray_body(Clause, Indent, indent, 1000, Out).
portray_body(Term, Indent, indent, Pri, Out) :- !, 
	nlindent(Out, Indent), 
	portray_body(Term, Indent, noindent, Pri, Out).
portray_body(Or, Indent, _, _, Out) :-
	or_layout(Or), !,
	write(Out, '(   '), 
	portray_or(Or, Indent, 1200, Out), 
	nlindent(Out, Indent), 
	write(Out, ')').
portray_body(Term, Indent, _, Pri, Out) :-
	term_needs_braces(Term, Pri), !,
	write(Out, '(   '), 
	portray_body(Term, Indent, noindent, 1200, Out), 
	nlindent(Out, Indent), 
	write(Out, ')').
portray_body((A,B), Indent, _, _Pri, Out) :- !, 
	infix_op((,), LeftPri, RightPri),
	portray_body(A, Indent, noindent, LeftPri, Out), 
	write(Out, ','), 
	portray_body(B, Indent, indent, RightPri, Out).
portray_body(\+(Goal), Indent, _, _Pri, Out) :- !,
	write(Out, (\+)), write(Out, ' '),
	prefix_op((\+), ArgPri),
	ArgIndent is Indent+3,
	portray_body(Goal, ArgIndent, noindent, ArgPri, Out).
portray_body(Meta, Indent, _, Pri, Out) :-
	meta_call(Meta, N), !, 
	portray_meta(Out, Meta, N, Indent, Pri).
portray_body(Clause, _, _, Pri, Out) :-
	pprint(Out, Clause, Pri).

term_needs_braces(Term, Pri) :-
	callable(Term),
	functor(Term, Name, _Arity),
	current_op(OpPri, _Type, Name),
	OpPri > Pri, !.

%%	portray_or(+Term, +Indent, +Priority, +Out) is det.

portray_or(Term, Indent, Pri, Out) :-
	term_needs_braces(Term, Pri), !,
	inc_indent(Indent, 1, NewIndent),
	write(Out, '(   '),
	portray_or(Term, NewIndent, Out),
	nlindent(Out, NewIndent),
	write(Out, ')').
portray_or(Term, Indent, _Pri, Out) :-
	or_layout(Term), !,
	portray_or(Term, Indent, Out).
portray_or(Term, Indent, Pri, Out) :-
	inc_indent(Indent, 1, NestIndent),
	portray_body(Term, NestIndent, noindent, Pri, Out).


portray_or((If -> Then ; Else), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out), 	
	nlindent(Out, Indent),
	write(Out, '->  '), 
	portray_body(Then, NestIndent, noindent, RightPri, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	infix_op(;, _LeftPri, RightPri2),
	portray_or(Else, Indent, RightPri2, Out).
portray_or((If *-> Then ; Else), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((*->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out), 	
	nlindent(Out, Indent),
	write(Out, '*-> '), 
	portray_body(Then, NestIndent, noindent, RightPri, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	infix_op(;, _LeftPri, RightPri2),
	portray_or(Else, Indent, RightPri2, Out).
portray_or((If -> Then), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out), 	
	nlindent(Out, Indent), 
	write(Out, '->  '), 
	portray_or(Then, Indent, RightPri, Out).
portray_or((If *-> Then), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out), 	
	nlindent(Out, Indent), 
	write(Out, '*-> '), 
	portray_or(Then, Indent, RightPri, Out).
portray_or((A;B), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((;), LeftPri, RightPri),
	portray_body(A, NestIndent, noindent, LeftPri, Out), 
	nlindent(Out, Indent), 
	write(Out, ';   '), 
	portray_or(B, Indent, RightPri, Out).
portray_or((A|B), Indent, Out) :- !, 
	inc_indent(Indent, 1, NestIndent),
	infix_op((|), LeftPri, RightPri),
	portray_body(A, NestIndent, noindent, LeftPri, Out), 	
	nlindent(Out, Indent), 
	write(Out, '|   '), 
	portray_or(B, Indent, RightPri, Out).


%%	infix_op(+Op, -Left, -Right) is semidet.
%
%	True if Op is an infix operator and Left is the max priority of its
%	left hand and Right is the max priority of its right hand.

infix_op(Op, Left, Right) :-
	current_op(Pri, Assoc, Op),
	infix_assoc(Assoc, LeftMin, RightMin), !,
	Left is Pri - LeftMin,
	Right is Pri - RightMin.

infix_assoc(xfx, 1, 1).
infix_assoc(xfy, 1, 0).
infix_assoc(yfx, 0, 1).
infix_assoc(yfy, 0, 0).

prefix_op(Op, ArgPri) :-
	current_op(Pri, Assoc, Op),
	pre_assoc(Assoc, ArgMin), !,
	ArgPri is Pri - ArgMin.

pre_assoc(fx, 1).
pre_assoc(fy, 0).

%%	or_layout(@Term) is semidet.
%
%	True if Term is a control structure for which we want to use clean
%	layout.
%	
%	@tbd	Change name.

or_layout(Var) :-
	var(Var), !, fail.
or_layout((_;_)).
or_layout((_->_)).
or_layout((_*->_)).

%%	meta_call(+Goal, -Arg) is semidet.
%
%	True if Goal is a meta-predicate for which we wish to format the
%	arguments.
%	
%	@tbd	This really must be  synchronised with meta_predicate
%		to fix this.

meta_call(call(_), 1).
meta_call(once(_), 1).
meta_call(not(_), 1).
meta_call(ignore(_), 1).

portray_meta(Out, Term, N, Indent, _Pri) :-
	arg(N, Term, Arg), 
	or_layout(Arg), !,
	functor(Term, Name, _), 
	format(Out, '~q((   ', [Name]),
	atom_length(Name, Alen),
	BraceIndent is Indent+Alen+1,
	portray_or(Arg, BraceIndent, Out), 
	nlindent(Out, BraceIndent), 
	write(Out, '))').	
portray_meta(Out, Term, _, _, Pri) :-
	pprint(Out, Term, Pri).	

%%	portray_list(+List, +Indent, +Out)
%	
%	Portray a list list this.  Right side for improper lists
%	
%		[ element1,		[ element1
%		  element2,	OR	| tail
%		]			]

portray_list([], _, Out) :- !,
	write(Out, []).
portray_list(List, Indent, Out) :-
	nlindent(Out, Indent),
	write(Out, '[ '),
	EIndent is Indent + 2,
	portray_list_elements(List, EIndent, Out),
	nlindent(Out, Indent),
	write(Out, ']').

portray_list_elements([H|T], EIndent, Out) :-
	pprint(Out, H, 999),
	(   T == []
	->  true
	;   nonvar(T), T = [_|_]
	->  write(Out, ','),
	    nlindent(Out, EIndent),
	    portray_list_elements(T, EIndent, Out)
	;   Indent is EIndent - 2,
	    nlindent(Out, Indent),
	    write(Out, '| '),
	    pprint(Out, T, 999)
	).

%%	nlindent(+Out, +Indent)
%	
%	Write newline and indent to column Indent.

nlindent(Out, N) :-
	nl(Out),
	Tab is N // 8, 
	Space is N mod 8,
	put_tabs(Out, Tab),
	tab(Out, Space).

put_tabs(Out, N) :-
	N > 0, !,
	put(Out, 0'\t),
	NN is N - 1,
	put_tabs(Out, NN).
put_tabs(_, _).


%%	inc_indent(+Indent0, +Inc, -Indent)
%	
%	Increment the indent with logical steps.

inc_indent(Indent0, Inc, Indent) :-
	Indent is Indent0 + Inc*4.

%%	pprint(+Out, +Term, +Priority)
%	
%	Print Term such that it can be read.

pprint(Out, Term, Pri) :-
	write_term(Out, Term,
		   [ quoted(true),
		     numbervars(true),
		     priority(Pri)
		   ]).
