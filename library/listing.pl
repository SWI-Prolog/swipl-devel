/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
			      VU University Amsterdam

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

:- module(prolog_listing,
	[ listing/0,
	  listing/1,
	  portray_clause/1,		% +Clause
	  portray_clause/2,		% +Stream, +Clause
	  portray_clause/3		% +Stream, +Clause, +Options
	]).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(error)).
:- set_prolog_flag(generate_debug_info, false).

:- module_transparent
	listing/0.
:- meta_predicate
	listing(:),
	portray_clause(+,+,:).

:- predicate_options(portray_clause/3, 3, [pass_to(system:write_term/3, 3)]).

:- multifile
	prolog:locate_clauses/2.	% +Spec, -ClauseRefList

/** <module> List programs and pretty print clauses

This module implements listing code from  the internal representation in
a human readable format.

    * listing/0 lists a module.
    * listing/1 lists a predicate or matching clause
    * portray_clause/2 pretty-prints a clause-term

Layout can be customized using library(settings). The effective settings
can be listed using list_settings/1 as   illustrated below. Settings can
be changed using set_setting/2.

    ==
    ?- list_settings(listing).
    ========================================================================
    Name                      Value (*=modified) Comment
    ========================================================================
    listing:body_indentation  8              Indentation used goals in the body
    listing:tab_distance      8              Distance between tab-stops.
    ...
    ==

@tbd	More settings, support _|Coding Guidelines for Prolog|_ and make
	the suggestions there the default.
@tbd	Provide persistent user customization
*/

:- setting(listing:body_indentation, nonneg, 8,
	   'Indentation used goals in the body').
:- setting(listing:tab_distance, nonneg, 8,
	   'Distance between tab-stops.  0 uses only spaces').
:- setting(listing:cut_on_same_line, boolean, true,
	   'Place cuts (!) on the same line').
:- setting(listing:line_width, nonneg, 78,
	   'Width of a line.  0 is infinite').


%%	listing
%
%	Lists all predicates defined  in   the  calling module. Imported
%	predicates are not listed. To  list   the  content of the module
%	=mymodule=, use:
%
%	  ==
%	  ?- mymodule:listing.
%	  ==

listing :-
	context_module(Context),
	current_predicate(_, Pred),
	\+ predicate_property(Pred, imported_from(_)),
	strip_module(Pred, Module, Head),
	functor(Head, Name, _Arity),
	(   (   predicate_property(Pred, built_in)
	    ;	sub_atom(Name, 0, _, _, $)
	    )
	->  current_prolog_flag(access_level, system)
	;   true
	),
	nl,
	list_predicate(Module:Head, Context),
	fail.
listing.


%%	listing(+What)
%
%	List matching clauses. What is either a plain specification or a
%	list of specifications. Plain specifications are:
%
%	  * Predicate indicator (Name/Arity or Name//Arity)
%	  Lists the indicated predicate.  This also outputs relevant
%	  _declarations_, such as multifile/1 or dynamic/1.
%
%	  * A _Head_ term.  In this case, only clauses whose head
%	  unify with _Head_ are listed.  This is illustrated in the
%	  query below that only lists the first clause of append/3.
%
%	    ==
%	    ?- listing(append([], _, _)).
%	    lists:append([], A, A).
%	    ==

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

%%	list_predicates(:Preds:list(pi), :Spec) is det.

list_predicates(PIs, Context:X) :-
	member(PI, PIs),
	pi_to_head(PI, Pred),
	unify_args(Pred, X),
	'$define_predicate'(Pred),
	strip_module(Pred, Module, Head),
        list_predicate(Module:Head, Context),
	nl,
        fail.
list_predicates(_, _).

pi_to_head(M:PI, M:Head) :- !,
	pi_to_head(PI, Head).
pi_to_head(Name/Arity, Head) :-
	functor(Head, Name, Arity).


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
	(   hide_module(Module, Context, Head)
	->  Decl = Name/Arity
	;   Decl = Module:Name/Arity
	).


decl(thread_local, thread_local).
decl(dynamic,	   dynamic).
decl(volatile,	   volatile).
decl(multifile,	   multifile).
decl(public,	   public).

declaration(Pred, Source, Decl) :-
	decl(Prop, Declname),
	predicate_property(Pred, Prop),
	decl_term(Pred, Source, Funct),
	Decl =.. [ Declname, Funct ].
declaration(Pred, Source, Decl) :- !,
	predicate_property(Pred, meta_predicate(Head)),
	strip_module(Pred, Module, _),
	(   (Module == system; Source == Module)
	->  Decl = meta_predicate(Head)
	;   Decl = meta_predicate(Module:Head)
	).
declaration(Pred, Source, Decl) :-
	predicate_property(Pred, transparent),
	decl_term(Pred, Source, PI),
	Decl = module_transparent(PI).

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
	    write_module(Module, Source, Head),
	    portray_clause((Head:-Body)),
	    fail
	;   true
	).

write_module(Module, Context, Head) :-
	hide_module(Module, Context, Head), !.
write_module(Module, _, _) :-
	format('~q:', [Module]).

hide_module(system, Module, Head) :-
	predicate_property(Module:Head, imported_from(M)),
	predicate_property(system:Head, imported_from(M)), !.
hide_module(Module, Module, _) :- !.

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
%%	portray_clause(+Out:stream, +Clause, +Options) is det.
%
%	Portray `Clause' on the current  output   stream.  Layout of the
%	clause is to our best standards.   As  the actual variable names
%	are not available we use A, B, ... Deals with ';', '|', '->' and
%	calls via meta-call predicates as determined using the predicate
%	property   meta_predicate.   If   Clause   contains   attributed
%	variables, these are treated as normal variables.
%
%	If  Options  is  provided,   the    option-list   is  passed  to
%	write_term/3 that does the final writing of arguments.

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
	must_be(stream, Stream),
	portray_clause(Stream, Term, []).

portray_clause(Stream, Term, M:Options) :-
	must_be(list, Options),
	meta_options(is_meta, M:Options, QOptions),
	\+ \+ ( copy_term_nat(Term, Copy),
		numbervars(Copy, 0, _,
			   [ singletons(true)
			   ]),
		do_portray_clause(Stream, Copy, QOptions)
	      ).

is_meta(portray_goal).

do_portray_clause(Out, Var, Options) :-
	var(Var), !,
	pprint(Out, Var, 1200, Options).
do_portray_clause(Out, (Head :- true), Options) :- !,
	pprint(Out, Head, 1200, Options),
	full_stop(Out).
do_portray_clause(Out, Term, Options) :-
	clause_term(Term, Head, Neck, Body), !,
	inc_indent(0, 1, Indent),
	infix_op(Neck, RightPri, LeftPri),
	pprint(Out, Head, LeftPri, Options),
	format(Out, ' ~w', [Neck]),
	(   nonvar(Body),
	    Body = Module:LocalBody,
	    \+ primitive(LocalBody)
	->  nlindent(Out, Indent),
	    format(Out, '~q', [Module]),
	    '$put_token'(Out, :),
	    nlindent(Out, Indent),
	    write(Out, '(   '),
	    inc_indent(Indent, 1, BodyIndent),
	    portray_body(LocalBody, BodyIndent, noindent, 1200, Out, Options),
	    nlindent(Out, Indent),
	    write(Out, ')')
	;   setting(listing:body_indentation, BodyIndent),
	    portray_body(Body, BodyIndent, indent, RightPri, Out, Options)
	),
	full_stop(Out).
do_portray_clause(Out, (:-use_module(File, Imports)), Options) :-
	length(Imports, Len),
	Len > 3, !,
	format(Out, ':- use_module(~q,', [File]),
	portray_list(Imports, 14, Out, Options),
	write(Out, ').\n').
do_portray_clause(Out, (:-module(Module, Exports)), Options) :- !,
	format(Out, ':- module(~q,', [Module]),
	portray_list(Exports, 10, Out, Options),
	write(Out, ').\n').
do_portray_clause(Out, (:-Directive), Options) :- !,
	write(Out, ':- '),
	portray_body(Directive, 3, noindent, 1199, Out, Options),
	full_stop(Out).
do_portray_clause(Out, Fact, Options) :-
	portray_body(Fact, 0, noindent, 1200, Out, Options),
	full_stop(Out).

clause_term((Head:-Body), Head, :-, Body).
clause_term((Head-->Body), Head, -->, Body).

full_stop(Out) :-
	'$put_token'(Out, '.'),
	nl(Out).


%%	portray_body(+Term, +Indent, +DoIndent, +Priority, +Out, +Options)
%
%	Write Term at current indentation. If   DoIndent  is 'indent' we
%	must first call nlindent/2 before emitting anything.

portray_body(Var, _, _, Pri, Out, Options) :-
	var(Var), !,
	pprint(Out, Var, Pri, Options).
portray_body(!, _, _, _, Out, _) :-
	setting(listing:cut_on_same_line, true), !,
	write(Out, ' !').
portray_body((!, Clause), Indent, _, Pri, Out, Options) :-
	setting(listing:cut_on_same_line, true),
	\+ term_needs_braces((_,_), Pri), !,
	write(Out, ' !,'),
	portray_body(Clause, Indent, indent, 1000, Out, Options).
portray_body(Term, Indent, indent, Pri, Out, Options) :- !,
	nlindent(Out, Indent),
	portray_body(Term, Indent, noindent, Pri, Out, Options).
portray_body(Or, Indent, _, _, Out, Options) :-
	or_layout(Or), !,
	write(Out, '(   '),
	portray_or(Or, Indent, 1200, Out, Options),
	nlindent(Out, Indent),
	write(Out, ')').
portray_body(Term, Indent, _, Pri, Out, Options) :-
	term_needs_braces(Term, Pri), !,
	write(Out, '( '),
	ArgIndent is Indent + 2,
	portray_body(Term, ArgIndent, noindent, 1200, Out, Options),
	nlindent(Out, Indent),
	write(Out, ')').
portray_body((A,B), Indent, _, _Pri, Out, Options) :- !,
	infix_op(',', LeftPri, RightPri),
	portray_body(A, Indent, noindent, LeftPri, Out, Options),
	write(Out, ','),
	portray_body(B, Indent, indent, RightPri, Out, Options).
portray_body(\+(Goal), Indent, _, _Pri, Out, Options) :- !,
	write(Out, \+), write(Out, ' '),
	prefix_op(\+, ArgPri),
	ArgIndent is Indent+3,
	portray_body(Goal, ArgIndent, noindent, ArgPri, Out, Options).
portray_body(Call, _, _, _, Out, Options) :- % requires knowledge on the module!
	m_callable(Call),
	option(module(M), Options, user),
	predicate_property(M:Call, meta_predicate(Meta)), !,
	portray_meta(Out, Call, Meta, Options).
portray_body(Clause, _, _, Pri, Out, Options) :-
	pprint(Out, Clause, Pri, Options).

m_callable(Term) :-
	strip_module(Term, _, Plain),
	callable(Plain),
	Plain \= (_:_).

term_needs_braces(Term, Pri) :-
	callable(Term),
	functor(Term, Name, _Arity),
	current_op(OpPri, _Type, Name),
	OpPri > Pri, !.

%%	portray_or(+Term, +Indent, +Priority, +Out) is det.

portray_or(Term, Indent, Pri, Out, Options) :-
	term_needs_braces(Term, Pri), !,
	inc_indent(Indent, 1, NewIndent),
	write(Out, '(   '),
	portray_or(Term, NewIndent, Out, Options),
	nlindent(Out, NewIndent),
	write(Out, ')').
portray_or(Term, Indent, _Pri, Out, Options) :-
	or_layout(Term), !,
	portray_or(Term, Indent, Out, Options).
portray_or(Term, Indent, Pri, Out, Options) :-
	inc_indent(Indent, 1, NestIndent),
	portray_body(Term, NestIndent, noindent, Pri, Out, Options).


portray_or((If -> Then ; Else), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, '->  '),
	portray_body(Then, NestIndent, noindent, RightPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, ';   '),
	infix_op(;, _LeftPri, RightPri2),
	portray_or(Else, Indent, RightPri2, Out, Options).
portray_or((If *-> Then ; Else), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op((*->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, '*-> '),
	portray_body(Then, NestIndent, noindent, RightPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, ';   '),
	infix_op(;, _LeftPri, RightPri2),
	portray_or(Else, Indent, RightPri2, Out, Options).
portray_or((If -> Then), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, '->  '),
	portray_or(Then, Indent, RightPri, Out, Options).
portray_or((If *-> Then), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op((->), LeftPri, RightPri),
	portray_body(If, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, '*-> '),
	portray_or(Then, Indent, RightPri, Out, Options).
portray_or((A;B), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op(;, LeftPri, RightPri),
	portray_body(A, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, ';   '),
	portray_or(B, Indent, RightPri, Out, Options).
portray_or((A|B), Indent, Out, Options) :- !,
	inc_indent(Indent, 1, NestIndent),
	infix_op('|', LeftPri, RightPri),
	portray_body(A, NestIndent, noindent, LeftPri, Out, Options),
	nlindent(Out, Indent),
	write(Out, '|   '),
	portray_or(B, Indent, RightPri, Out, Options).


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

prefix_op(Op, ArgPri) :-
	current_op(Pri, Assoc, Op),
	pre_assoc(Assoc, ArgMin), !,
	ArgPri is Pri - ArgMin.

pre_assoc(fx, 1).
pre_assoc(fy, 0).

postfix_op(Op, ArgPri) :-
	current_op(Pri, Assoc, Op),
	post_assoc(Assoc, ArgMin), !,
	ArgPri is Pri - ArgMin.

post_assoc(xf, 1).
post_assoc(yf, 0).

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

primitive(G) :-
	or_layout(G), !, fail.
primitive((_,_)) :- !, fail.
primitive(_).


%%	portray_meta(+Out, +Call, +MetaDecl, +Options)
%
%	Portray a meta-call. If Call   contains non-primitive meta-calls
%	we put each argument on a line and layout the body. Otherwise we
%	simply print the goal.

portray_meta(Out, Call, Meta, Options) :-
	contains_non_primitive_meta_arg(Call, Meta), !,
	Call =.. [Name|Args],
	Meta =.. [_|Decls],
	format(Out, '~q(', [Name]),
	line_position(Out, Indent),
	portray_meta_args(Decls, Args, Indent, Out, Options),
	format(Out, ')', []).
portray_meta(Out, Call, _, Options) :-
	pprint(Out, Call, 999, Options).

contains_non_primitive_meta_arg(Call, Decl) :-
	arg(I, Call, CA),
	arg(I, Decl, DA),
	integer(DA),
	\+ primitive(CA), !.

portray_meta_args([], [], _, _, _).
portray_meta_args([D|DT], [A|AT], Indent, Out, Options) :-
	portray_meta_arg(D, A, Out, Options),
	(   DT == []
	->  true
	;   format(Out, ',', []),
	    nlindent(Out, Indent),
	    portray_meta_args(DT, AT, Indent, Out, Options)
	).

portray_meta_arg(I, A, Out, Options) :-
	integer(I), !,
	line_position(Out, Indent),
	portray_body(A, Indent, noindent, 999, Out, Options).
portray_meta_arg(_, A, Out, Options) :-
	pprint(Out, A, 999, Options).

%%	portray_list(+List, +Indent, +Out)
%
%	Portray a list like this.  Right side for improper lists
%
%		[ element1,		[ element1
%		  element2,	OR	| tail
%		]			]

portray_list([], _, Out, _) :- !,
	write(Out, []).
portray_list(List, Indent, Out, Options) :-
	nlindent(Out, Indent),
	write(Out, '[ '),
	EIndent is Indent + 2,
	portray_list_elements(List, EIndent, Out, Options),
	nlindent(Out, Indent),
	write(Out, ']').

portray_list_elements([H|T], EIndent, Out, Options) :-
	pprint(Out, H, 999, Options),
	(   T == []
	->  true
	;   nonvar(T), T = [_|_]
	->  write(Out, ','),
	    nlindent(Out, EIndent),
	    portray_list_elements(T, EIndent, Out, Options)
	;   Indent is EIndent - 2,
	    nlindent(Out, Indent),
	    write(Out, '| '),
	    pprint(Out, T, 999, Options)
	).

%%	pprint(+Out, +Term, +Priority, +Options)
%
%	Print  Term  at  Priority.  This  also  takes  care  of  several
%	formatting options, in particular:
%
%	  * {}(Arg) terms are printed with aligned arguments, assuming
%	  that the term is a body-term.
%	  * Terms that do not fit on the line are wrapped using
%	  pprint_wrapped/3.
%
%	@tbd	Decide when and how to wrap long terms.

pprint(Out, Term, _, Options) :-
	nonvar(Term),
	Term = {}(Arg),
	line_position(Out, Indent),
	ArgIndent is Indent + 2,
	format(Out, '{ ', []),
	portray_body(Arg, ArgIndent, noident, 1000, Out, Options),
	nlindent(Out, Indent),
	format(Out, '}', []).
pprint(Out, Term, Pri, Options) :-
	compound(Term),
	\+ nowrap_term(Term),
	setting(listing:line_width, Width),
	Width > 0,
	(   write_length(Term, Len, [max_length(Width)|Options])
	->  true
	;   Len = Width
	),
	line_position(Out, Indent),
	Indent + Len > Width,
	Len > Width/4, !,		% ad-hoc rule for deeply nested goals
	pprint_wrapped(Out, Term, Pri, Options).
pprint(Out, Term, Pri, Options) :-
	listing_write_options(Pri, WrtOptions, Options),
	write_term(Out, Term, WrtOptions).

nowrap_term('$VAR'(_)) :- !.
nowrap_term(Term) :-
	functor(Term, Name, Arity),
	current_op(_, _, Name),
	(   Arity == 2
	->  infix_op(Name, _, _)
	;   Arity == 1
	->  (   prefix_op(Name, _)
	    ->	true
	    ;	postfix_op(Name, _)
	    )
	).


pprint_wrapped(Out, Term, _, Options) :-
	Term = [_|_], !,
	line_position(Out, Indent),
	portray_list(Term, Indent, Out, Options).
pprint_wrapped(Out, Term, _, Options) :-
	Term =.. [Name|Args],
	format(Out, '~q(', Name),
	line_position(Out, Indent),
	pprint_args(Args, Indent, Out, Options),
	format(Out, ')', []).

pprint_args([], _, _, _).
pprint_args([H|T], Indent, Out, Options) :-
	pprint(Out, H, 999, Options),
	(   T == []
	->  true
	;   format(Out, ',', []),
	    nlindent(Out, Indent),
	    pprint_args(T, Indent, Out, Options)
	).


%%	listing_write_options(+Priority, -WriteOptions) is det.
%
%	WriteOptions are write_term/3 options for writing a term at
%	priority Priority.

listing_write_options(Pri,
		      [ quoted(true),
			numbervars(true),
			priority(Pri),
			spacing(next_argument)
		      | Options
		      ],
		      Options).

%%	nlindent(+Out, +Indent)
%
%	Write newline and indent to  column   Indent.  Uses  the setting
%	listing:tab_distance to determine the mapping   between tabs and
%	spaces.

nlindent(Out, N) :-
	nl(Out),
	setting(listing:tab_distance, D),
	(   D =:= 0
	->  tab(Out, N)
	;   Tab is N // D,
	    Space is N mod D,
	    put_tabs(Out, Tab),
	    tab(Out, Space)
	).

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

