/*  $Id pce_expansion.pl,v 1.6 1995/09/25 15:33:02 jan Exp $

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(pce_expansion,
	[ pce_term_expansion/2,		% +In, -Out
	  pce_compiling/1,		% -ClassName
	  pce_begin_recording/1,	% +- source|documentation
	  pce_end_recording/0
	]).
:- use_module(pce_operator).
:- use_module(pce_principal).
:- require([ append/3
	   , between/3
	   , concat/3
	   , concat_atom/2
	   , file_base_name/2
	   , flatten/2
	   , forall/2
	   , genarg/3
	   , maplist/3
	   , pce_error/1
	   , pce_info/1
	   , pce_warn/1
	   , reverse/2
	   , source_location/2
	   , term_to_atom/2
	   ]).

:- dynamic
	compiling/1,			% -ClassName
	attribute/3,			% ClassName, Attribute, Value
	verbose/0,
	recording/2.			% items recorded

pce_ifhostproperty(prolog(swi), (:- index(attribute(1,1,0)))).

pce_ifhostproperty(prolog(sicstus),
[(get(R, S, A)	     :- pce_principal:get(R, S, A)),
 (get(R, S, P1, A)   :- pce_principal:get(R, S, P1, A)),
 (send(R, S, P1, P2) :- pce_principal:send(R, S, P1, P2))
]).

pce_ifhostproperty(prolog(quintus),
[(get(R, S, A)	     :- pce_principal:get(R, S, A)),
 (get(R, S, P1, A)   :- pce_principal:get(R, S, P1, A)),
 (send(R, S, P1, P2) :- pce_principal:send(R, S, P1, P2))
]).

		 /*******************************
		 *	     OPERATORS		*
		 *******************************/

%	push_compile_operators.
%	Push the current 

push_compile_operators :-
	push_operators(
		[ op(1200, xfx, :->)
		, op(1200, xfx, :<-)
		, op(1190, xfx, ::)
		, op(100,  xf,  *)
		, op(125,  xf,  ?)
		, op(150,  xf,  ...)
		, op(100,  xfx, ..)
		]).

pop_compile_operators :-
	pop_operators.

:- op(100, fx, @).			% standard XPCE operators
:- op(150, yfx, ?).
:- op(990, xfx, :=).
:- push_compile_operators.

%verbose.

pce_term_expansion(In, Out) :-
	pce_pre_expand(In, In0),
	(   list(In0)
	->  maplist(map_term_expand, In0, In1),
	    flatten(In1, Out0),
	    (	Out0 = [X]
	    ->	Out = X
	    ;	Out = Out0
	    )
	;   do_term_expand(In0, Out)
	).

list([]).
list([_|_]).

map_term_expand(X, Y) :-
	do_term_expand(X, Y), !.
map_term_expand(X, X).


do_term_expand(In0, Out) :-
	pce_expandable(In0),
	(   do_expand(In0, Out0)
	->  (   pce_post_expand(Out0, Out)
	    ->	true
	    ;	Out = Out0
	    )
	;   pce_error(expand_failed(In0)),
	    Out = []
	), !.
do_term_expand((Head :- Body), _) :-	% check for :- instead of :-> or :<-
	pce_compiling,
	(   Body = ([A|B] :: _Body),
	    is_string([A|B])
	;   typed_head(Head)
	),
	pce_error(context_error((Head :- Body), nomethod, clause)),
	fail.

	
is_string([]).
is_string([H|T]) :-
	integer(H),
	between(0, 255, H),
	is_string(T).

typed_head(T) :-
	functor(T, _, Arity),
	Arity > 1,
	forall(genarg(N, T, A), head_arg(N, A)).

head_arg(1, A) :- !,
	var(A).
head_arg(_, A) :-
	nonvar(A),
	(   A = (_Var:TP)
	->  true
	;   A = (_Var:Name=TP),
	    atom(Name)
	),
	ground(TP).

%	pce_pre_expand(+In, -Out)
%
%	First step of the XPCE class compiler, calling the supported
%	hook pce_pre_expansion_hook/2.

:- multifile user:pce_pre_expansion_hook/2.
:- dynamic user:pce_pre_expansion_hook/2.
:- multifile user:pce_post_expansion_hook/2.
:- dynamic user:pce_post_expansion_hook/2.

pce_pre_expand(X, Y) :-
	user:pce_pre_expansion_hook(X, X1), !,
	do_pce_pre_expand(X1, Y).
pce_pre_expand(X, Y) :-
	do_pce_pre_expand(X, Y).

do_pce_pre_expand((:- pce_begin_class(Class, Super)),
		  (:- pce_begin_class(Class, Super, @default))).
do_pce_pre_expand(variable(Name, Type, Access),
		  variable(Name, Type, Access, @default)) :-
	pce_compiling.
do_pce_pre_expand(class_variable(Name, Type, Default),
		  class_variable(Name, Type, Default, @default)) :-
	pce_compiling.
do_pce_pre_expand(handle(X, Y, Kind),
		  handle(X, Y, Kind, @default)) :-
	pce_compiling.
do_pce_pre_expand((:- ClassDirective), D) :-
	functor(ClassDirective, send, _),
	arg(1, ClassDirective, @class), !,
	D = (:- pce_class_directive(ClassDirective)).
do_pce_pre_expand(pce_ifhostproperty(Prop, Clause), TheClause) :-
	(   pce_host:property(Prop)
	->  TheClause = Clause
	;   TheClause = []
	).
do_pce_pre_expand(pce_ifhostproperty(Prop, If, Else), Clause) :-
	(   pce_host:property(Prop)
	->  Clause = If
	;   Clause = Else
	).
do_pce_pre_expand(X, X).


%	pce_post_expand(In, Out)

pce_post_expand([], []).
pce_post_expand([H0|T0], [H|T]) :-
	user:pce_post_expansion_hook(H0, H), !,
	pce_post_expand(T0, T).
pce_post_expand([H|T0], [H|T]) :-
	pce_post_expand(T0, T).
pce_post_expand(T0, T) :-
	user:pce_post_expansion_hook(T0, T), !.
pce_post_expand(T, T).


%	pce_expandable(+Term)
%	Quick test whether we can expand this.

pce_expandable((:- pce_begin_class(_Class, _Super, _Doc))).
pce_expandable((:- pce_extend_class(_Class))).
pce_expandable((:- pce_end_class)).
pce_expandable((:- use_class_template(_TemplateClass))).
pce_expandable((:- pce_group(_))).
pce_expandable((:- pce_class_directive(_))).
pce_expandable(variable(_Name, _Type, _Access, _Doc)) :-
	pce_compiling.
pce_expandable(class_variable(_Name, _Type, _Default, _Doc)) :-
	pce_compiling.
pce_expandable(delegate_to(_VarName)) :-
	pce_compiling.
pce_expandable(handle(_X, _Y, _Kind, _Name)) :-
	pce_compiling.
pce_expandable((_Head :-> _Body)).
pce_expandable((_Head :<- _Body)).


%	do_expand(In, Out)
%
%	The XPCE kernel expansion.

do_expand((:- pce_begin_class(Spec, Super, Doc)), []) :-
	break_class_specification(Spec, ClassName, MetaClass, TermArgs),
	push_class(ClassName),
	set_attribute(ClassName, super, Super),
	set_attribute(ClassName, meta, MetaClass),
	class_summary(ClassName, Doc),
	class_source(ClassName),
	term_names(ClassName, TermArgs).
do_expand((:- pce_extend_class(ClassName)), []) :-
	push_class(ClassName),
	set_attribute(ClassName, extending, true).
do_expand((:- pce_end_class),
	  [ pce_principal:pce_class(ClassName, MetaClass, Super,
				    Variables,
				    Resources,
				    Directs),
	    RegisterDecl
	  ]) :-
	pce_compiling(ClassName),
	findall(V, retract(attribute(ClassName, variable, V)),  Variables),
	findall(R, retract(attribute(ClassName, classvar, R)),  Resources),
	findall(D, retract(attribute(ClassName, directive, D)), Directs),
	(   attribute(ClassName, extending, true)
	->  MetaClass = '-',
	    Super = '-',
	    expand_term((:- initialization(pce_extended_class(ClassName))),
			RegisterDecl)
	;   retract(attribute(ClassName, super, Super)),
	    retract(attribute(ClassName, meta, MetaClass)),
	    expand_term((:- initialization(pce_register_class(ClassName))),
			RegisterDecl)
	),
	pop_class.
do_expand((:- use_class_template(Template)), []) :-
	used_class_template(Template), !.
do_expand((:- use_class_template(Template)),
	  [ pce_principal:pce_uses_template(ClassName, Template)
	  | LinkClauses
	  ]) :-
	pce_compiling(ClassName),
	use_template_class_attributes(Template),
	use_template_send_methods(Template, SendClauses),
	use_template_get_methods(Template, GetClauses),
	append(SendClauses, GetClauses, LinkClauses).
do_expand((:- pce_group(Group)), []) :-
	pce_compiling(ClassName),
	set_attribute(ClassName, group, Group).
do_expand(variable(Name, Type, Access, Doc), []) :-
	pce_compiling(ClassName),
	current_group(ClassName, Group),
	pce_access(Access),
	var_type(Type, PceType, Initial),
	pce_summary(Doc, PceDoc),
	strip_defaults([Initial, Group, PceDoc], Defs),
	Var =.. [variable, Name, PceType, Access | Defs],
	add_attribute(ClassName, variable, Var).
do_expand(class_variable(Name, Type, Default, Doc), []) :-
	pce_compiling(ClassName),
	pce_type(Type, PceType),
	pce_summary(Doc, PceDoc),
	add_attribute(ClassName, classvar,
		      class_variable(Name, Default, PceType, PceDoc)).
do_expand(handle(X, Y, Kind, Name), []) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, directive,
		      send(@class, handle, handle(X, Y, Kind, Name))).
do_expand(delegate_to(Var), []) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, directive,
		      send(@class, delegate, Var)).
do_expand((:- pce_class_directive(Goal)),
	  (:- initialization((send(@class, assign, Class),
			      Goal)))) :-
	pce_compiling(ClassName),
	realised_class(ClassName),
	attribute(ClassName, extending, true), !,
	get(@classes, member, ClassName, Class).
do_expand((:- pce_class_directive(Goal)), (:- Goal)) :-
	pce_compiling(ClassName),
	realised_class(ClassName), !.
do_expand((:- pce_class_directive(Goal)), []) :-
	pce_compiling(ClassName),
	prolog_load_context(module, M),
	add_attribute(ClassName, directive, M:Goal).
do_expand((Head :-> DocBody),
	  [ pce_principal:pce_lazy_send_method(Selector, ClassName, LSM)
	  | Clauses
	  ]) :-
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	pce_compiling(ClassName),
	current_group(ClassName, Group),
	prolog_head(send, Id, Head, Selector, Types, PlHead),
	strip_defaults([Group, Loc, Doc], NonDefArgs),
	LSM =.. [bind_send, Id, Types | NonDefArgs],
	Clause = (PlHead :- Body),
	gen_method_id((->), ClassName, Selector, Id),
	(   attribute(ClassName, super, template)
	->  template_clause(Clause, Clauses)
	;   Clauses = [Clause]
	),
	(   realised_class(ClassName)	% force a reload (TBD: move to realise)
	->  send(@class, delete_send_method, Selector)
	;   true
	),
	feedback(expand_send(ClassName, Selector)).
do_expand((Head :<- DocBody),
	  [ pce_principal:pce_lazy_get_method(Selector, ClassName, LGM)
	  | Clauses
	  ]) :-
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	pce_compiling(ClassName),
	current_group(ClassName, Group),
	return_type(Head, RType),
	prolog_head(get, Id, Head, Selector, Types, PlHead),
	strip_defaults([Group, Loc, Doc], NonDefArgs),
	LGM =.. [bind_get, Id, RType, Types | NonDefArgs],
	Clause = (PlHead :- Body),
	gen_method_id((<-), ClassName, Selector, Id),
	(   attribute(ClassName, super, template)
	->  template_clause(Clause, Clauses)
	;   Clauses = [Clause]
	),
	(   realised_class(ClassName)	% force a reload
	->  send(@class, delete_get_method, Selector)
	;   true
	),
	feedback(expand_get(ClassName, Selector)).

strip_defaults([@default|T0], T) :- !,
	strip_defaults(T0, T).
strip_defaults(L, LV) :-
	reverse(L, LV).

break_class_specification(Meta:Term, ClassName, Meta, TermArgs) :- !,
	Term =.. [ClassName|TermArgs].
break_class_specification(Term, ClassName, @default, TermArgs) :-
	Term =.. [ClassName|TermArgs].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
gen_method_id(+SendGet, +Class, +Selector, -Identifier)

Generate a unique identifier for the method,  used as the first argument
of send_implementation/3 or get_implementation/4.  The identifier should
be an atom or integer. The  value  is   not  relevant,  as long as it is
unique.

This  suggests  simple  counting:  always    unique   and  integers  are
considerably cheaper than atoms. Unfortunately, there  is a problem with
this. If methods appear in pre-compiled files, they cannot be joined. It
is hard to see a good and workable  solution to this problem. Grant each
file a domain? How do we associate a unique domain to each file?
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

gen_method_id(SG, Class, Selector, Id) :-
	attribute(Class, extending, true), !,
	concat_atom([Class, '$+$', SG, Selector], Id).
gen_method_id(SG, Class, Selector, Id) :-
	concat_atom([Class, SG, Selector], Id).

%gen_method_id(_, _, _, Id) :-
%	flag(pce_method_id, Id, Id+1).

		 /*******************************
		 *       TEMPLATE SUPPORT	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
When compiling a template, calls to   send_class/3 and get_class/4 refer
to the template  classes.  This  is   not  correct.  Therefore,  we will
translate  the  method  implementation   into    a   parameterized  real
implementation and a normal implementation  that calls the parameterized
one. On method instantiation, we create additional clauses for the class
to which we attach the method.

Importing the template (pce_use_class_template/1):

	+ Put binding in bind_lazy by searching the templates.
	+ Expand the directive itself into the wrapper-implementations.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

template_clause((M:send_implementation(Id, Msg, R) :- Body),
		[ (M:send_implementation(Tid, ClassMsg, R) :- ClassBody),
		  (M:(send_implementation(Id, Msg, R) :-
		  	send_implementation(Tid, IClassMsg, R)))
		]) :- !,
	concat('T-', Id, Tid),
	Msg =.. Args,
	append(Args, [Class], Args2),
	ClassMsg =.. Args2,
	append(Args, [template], Args3),
	IClassMsg =.. Args3,
	template_body(Body, template, Class, ClassBody).
template_clause((M:get_implementation(Id, Msg, R, V) :- Body),
		[ (M:get_implementation(Tid, ClassMsg, R, V) :- ClassBody),
		  (M:(get_implementation(Id, Msg, R, V) :-
		  	get_implementation(Tid, IClassMsg, R, V)))
		]) :- !,
	concat('T-', Id, Tid),
	Msg =.. Args,
	append(Args, [Class], Args2),
	ClassMsg =.. Args2,
	append(Args, [template], Args3),
	IClassMsg =.. Args3,
	template_body(Body, template, Class, ClassBody).
template_clause(Clause, Clause).

template_body(G0, T, C, G) :-
	compound(G0),
	functor(G0, Name, Arity),
	functor(M, Name, Arity),
	meta(M), !,
	functor(G, Name, Arity),
	convert_meta(0, Arity, G0, M, T, C, G).
template_body(G, T, C, send_class(R, C, Msg)) :-
	expand_goal(G, send_class(R, T, Msg)), !.
template_body(G, T, C, get_class(R, C, Msg, V)) :-
	expand_goal(G, get_class(R, T, Msg, V)), !.
template_body(G, _, _, G).

convert_meta(A, A, _, _, _, _, _) :- !.
convert_meta(I, Arity, G0, M, T, C, G) :-
	A is I + 1,
	arg(A, M, :), !,
	arg(A, G0, GA0),
	arg(A, G,  GA),
	template_body(GA0, T, C, GA),
	convert_meta(A, Arity, G0, M, T, C, G).
convert_meta(I, Arity, G0, M, T, C, G) :-
	A is I + 1,
	arg(A, G0, GA),
	arg(A, G,  GA),
	convert_meta(A, Arity, G0, M, T, C, G).

meta(','(:, :)).	
meta(;(:, :)).	
meta(->(:, :)).	
meta(*->(:, :)).	
meta(\+(:)).	
meta(not(:)).	
meta(forall(:, :)).	
meta(call(:)).	

%	use_template_class_attributes(+Template)
%
%	Insert variables, class-variables and directives as if they appeared
%	in the current class definition.

use_template_class_attributes(Template) :-
	pce_class(Template, _, template, Variables, ClassVars, Directs),
	assert_attributes(Variables, variable),
	assert_attributes(ClassVars, classvar),
	assert_attributes(Directs,   directive).

assert_attributes([], _).
assert_attributes([H|T], Att) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, Att, H),
	assert_attributes(T, Att).

use_template_send_methods(Template, Clauses) :-
	findall(C, use_template_send_method(Template, C), Clauses).

use_template_send_method(Template, pce_principal:Clause) :-
	pce_compiling(ClassName),
	pce_lazy_send_method(Sel, Template, Binder),
	Binder =.. [Functor, Id | RestBinder],
	gen_method_id('$T$->', ClassName, Sel, NewId),
	(   Clause = pce_lazy_send_method(Sel, ClassName, NewBinder),
	    NewBinder =.. [Functor, NewId | RestBinder]
	;   Clause = (send_implementation(NewId, Msg, R) :-
		     	send_implementation(Tid, IClassMsg, R)),
	    attribute(ClassName, super, SuperClass), % TBD: pce_extend_class/1
	    arg(2, Binder, Types),
	    type_arity(Types, Arity),
	    functor(Msg, Sel, Arity),
	    Msg =.. Args,
	    append(Args, [SuperClass], Args1),
	    IClassMsg =.. Args1,
	    concat('T-', Id, Tid)
	).
	  
use_template_get_methods(Template, Clauses) :-
	findall(C, use_template_get_method(Template, C), Clauses).

use_template_get_method(Template, pce_principal:Clause) :-
	pce_compiling(ClassName),
	pce_lazy_get_method(Sel, Template, Binder),
	Binder =.. [Functor, Id | RestBinder],
	gen_method_id('$T$<-', ClassName, Sel, NewId),
	(   Clause = pce_lazy_get_method(Sel, ClassName, NewBinder),
	    NewBinder =.. [Functor, NewId | RestBinder]
	;   Clause = (get_implementation(NewId, Msg, R, V) :-
		     	get_implementation(Tid, IClassMsg, R, V)),
	    attribute(ClassName, super, SuperClass), % TBD: pce_extend_class/1
	    arg(3, Binder, Types),
	    type_arity(Types, Arity),
	    functor(Msg, Sel, Arity),
	    Msg =.. Args,
	    append(Args, [SuperClass], Args1),
	    IClassMsg =.. Args1,
	    concat('T-', Id, Tid)
	).

type_arity(@default, 0) :- !.
type_arity(Atom, 1) :-
	atom(Atom), !.
type_arity(Vector, A) :-
	functor(Vector, _, A).

%	used_class_template(+Template)
%
%	Succeeds if any of my (Prolog-defined) super classes
%	has imported the named template.

used_class_template(Template) :-
	pce_compiling(Class),
	isa_prolog_class(Class, Super),
	Super \== Class,
	pce_uses_template(Super, Template), !.

isa_prolog_class(Class, Class).
isa_prolog_class(Class, Super) :-
	attribute(Class, super, Super0), !,	% Prolog class being loaded
	isa_prolog_class(Super0, Super).
isa_prolog_class(Class, Super) :-		% Loaded Prolog class
	pce_class(Class, _, Super0, _, _, _), !,
	isa_prolog_class(Super0, Super).


		 /*******************************
		 *   PUSH/POP CLASS STRUCTURE	*
		 *******************************/

%	push_class(+ClassName)
%	Start compiling the argument class.

push_class(ClassName) :-
	compiling(ClassName), !,
	pce_error(resursive_loading_class(ClassName)),
	fail.
push_class(ClassName) :-
	(   compiling(_)
	->  true
	;   push_compile_operators
	),
	asserta(compiling(ClassName)),
	(   realised_class(ClassName)
	->  get(@class, '_value', OldClassVal),
	    asserta(attribute(ClassName, old_class_val, OldClassVal)),
	    get(@classes, member, ClassName, Class),
	    send(@class, assign, Class, global)
	;   true
	).

%	pop_class
%	End class compilation.

pop_class :-
	retract(compiling(ClassName)), !,
	(   attribute(ClassName, old_class_val, OldClassVal)
	->  send(@class, assign, OldClassVal, global)
	;   true
	),
	retractall(attribute(ClassName, _, _)),
	(   compiling(_)
	->  true
	;   pop_compile_operators
	).
pop_class :-
	pce_error(no_class_to_end),
	fail.
	
		 /*******************************
		 *	     ATTRIBUTES		*
		 *******************************/

set_attribute(Class, Name, Value) :-
	retractall(attribute(Class, Name, _)),
	asserta(attribute(Class, Name, Value)).

add_attribute(Class, Name, Value) :-
	assert(attribute(Class, Name, Value)).


		 /*******************************
		 *	  CONTEXT VALUES	*
		 *******************************/

source_location_term(source_location(File, Line)) :-
	pce_recording(source),
	source_location(File, Line), !.
source_location_term(@default).

current_group(Class, Group) :-
	attribute(Class, group, Group), !.
current_group(_, @default).

class_source(ClassName) :-
	pce_recording(source),
	source_location_term(Term),
	Term \== @default, !,
	add_attribute(ClassName, directive,
		      send(@class, source, Term)).
class_source(_).
	

		 /*******************************
		 *	     RECORDING		*
		 *******************************/

pce_begin_recording(+Topic) :-
	asserta(recording(Topic, true)).
pce_begin_recording(-Topic) :-
	asserta(recording(Topic, fail)).

pce_end_recording :-
	retract(recording(_, _)), !.

pce_recording(Topic) :-
	recording(Topic, X), !,
	X == true.
pce_recording(_).			% default recording all


		 /*******************************
		 *	  SUMMARY HANDLING	*
		 *******************************/

class_summary(ClassName, Summary) :-
	pce_summary(Summary, PceSummary),
	(   PceSummary \== @default
	->  add_attribute(ClassName, directive,
			  send(@class, summary, PceSummary))
	;   true
	).


pce_summary(@X, @X) :- !.
pce_summary(_, @default) :-
	\+ pce_recording(documentation), !.
pce_summary(Atomic, Atomic) :-
	atomic(Atomic), !.
pce_ifhostproperty(string, [
(pce_summary(String, String) :-
	string(String), !),
(pce_summary(List, String) :-
	string_to_list(String, List))]).
pce_summary(List, string(List)).


		 /*******************************
		 *	 TERM DESCRIPTION	*
		 *******************************/

term_names(_, []) :- !.
term_names(Class, Selectors) :-
	VectorTerm =.. [vector|Selectors],
	add_attribute(Class, directive,
		      send(@class, term_names, new(VectorTerm))).


		 /*******************************
		 *	       ACCESS		*
		 *******************************/

pce_access(both) :- !.
pce_access(get)  :- !.
pce_access(send) :- !.
pce_access(none) :- !.
pce_access(X) :-
	pce_error(invalid_access(X)),
	fail.


		 /*******************************
		 *	       TYPES		*
		 *******************************/

%	pce_type(+Spec, -PceTypeName)
%	Convert type specification into legal PCE type-name

pce_type(Prolog, Pce) :-
	to_atom(Prolog, RawPce),
	canonise_type(RawPce, Pce).

canonise_type(T0, T0) :-
	concat(_, ' ...', T0), !.
canonise_type(T0, T) :-
	concat(T1, '...', T0), !,
	concat(T1, ' ...', T).
canonise_type(T, T).

to_atom(Atom, Atom) :-
	atom(Atom), !.
to_atom(Term, Atom) :-
	ground(Term), !,
	phrase(pce_type_description(Term), Chars, []),
	atom_chars(Atom, Chars).
to_atom(Term, any) :-
	pce_error(type_error(to_atom(Term, any), 1, ground, Term)).

pce_type_description(Atom, Chars, Tail) :-
	atomic(Atom), !,
	name(Atom, C0),
	append(C0, Tail, Chars).
pce_type_description([X]) -->
	"[", pce_type_description(X), "]".
pce_type_description([X|Y]) -->
	"[", pce_type_description(X), "|", pce_type_description(Y), "]".
pce_type_description({}(Words)) -->
	"{", word_list(Words), "}".
pce_type_description(=(Name, Type)) -->
	pce_type_description(Name), "=", pce_type_description(Type).
pce_type_description(*(T)) -->
	pce_type_description(T), "*".
pce_type_description(...(T)) -->
	pce_type_description(T), " ...".

word_list((A,B)) --> !,
	pce_type_description(A), ",", word_list(B).
word_list(A) -->
	pce_type_description(A).


var_type(Type := Initial, PceType, Initial) :- !,
	pce_type(Type, PceType).
var_type(Type, PceType, @default) :-
	pce_type(Type, PceType).


		 /*******************************
		 *	  METHOD SUPPORT	*
		 *******************************/

extract_documentation((DocText::Body), Summary, Body) :- !,
	pce_summary(DocText, Summary).
extract_documentation((DocText,Body), Summary, Body) :-
	is_string(DocText), !,
	pce_summary(DocText, Summary),
	pce_warn(summary_not_closed(DocText)).
extract_documentation(Body, @default, Body).


return_type(Term, RType) :-
	functor(Term, _, Arity),
	arg(Arity, Term, Last),
	(   nonvar(Last),
	    Last = _:Type
	->  pce_type(Type, RType)
	;   RType = @default
	).

prolog_head(send, MethodId, Head, Selector,
	    TypeVector, pce_principal:PlHead) :- !,
	Head =.. [Selector, Receiver | Args],
	prolog_send_arguments(Args, Types, PlArgs),
	create_type_vector(Types, TypeVector),
	CallArgs =.. [Selector | PlArgs],
	PlHead =.. [send_implementation, MethodId, CallArgs, Receiver].
prolog_head(get, MethodId, Head, Selector,
	    TypeVector, pce_principal:PlHead) :- !,
	Head =.. [Selector, Receiver | Args],
	prolog_get_arguments(Args, Types, PlArgs, Rval),
	create_type_vector(Types, TypeVector),
	CallArgs =.. [Selector | PlArgs],
	PlHead =.. [get_implementation, MethodId, CallArgs, Receiver, Rval].

create_type_vector([],      @default) :- !.
create_type_vector(List,    VectorTerm) :-
	VectorTerm =.. [vector|List].

prolog_send_arguments([], [], []) :- !.
prolog_send_arguments([ArgAndType|RA], [T|RT], [Arg|TA]) :- !,
	head_arg(ArgAndType, Arg, Type),
	pce_type(Type, T),
	prolog_send_arguments(RA, RT, TA).

prolog_get_arguments([Return], [], [], ReturnVar) :- !,
	(   var(Return)
	->  ReturnVar = Return
	;   Return = ReturnVar:_Type
	).
prolog_get_arguments([ArgAndType|RA], [T|RT], [Arg|TA], ReturnVar) :- !,
	head_arg(ArgAndType, Arg, Type),
	pce_type(Type, T),
	prolog_get_arguments(RA, RT, TA, ReturnVar).


head_arg(Var, Var, any) :-
	var(Var), !.
head_arg(Arg:Type, Arg, Type).
head_arg(Arg:Name=Type, Arg, Name=Type).

	
		 /*******************************
		 *	  PUBLIC METHODS	*
		 *******************************/

%	pce_compiling(-ClassName)
%	External function to get the current classname

pce_compiling(ClassName) :-
	compiling(X), !,
	X = ClassName.

pce_compiling :-
	compiling(_), !.


		 /*******************************
		 *	      CHECKS		*
		 *******************************/

pce_ifhostproperty(qpc,
(realised_class(_ClassName) :- fail),
(realised_class(ClassName) :-
	get(@classes, member, ClassName, Class),
	get(Class, realised, @on))).


		/********************************
		*           UTILITIES		*
		********************************/

term_member(El, Term) :-
	El == Term.
term_member(El, Term) :-
	functor(Term, _, Arity),
	term_member(Arity, El, Term).

term_member(0, _, _) :- !,
	fail.
term_member(N, El, Term) :-
	arg(N, Term, Sub),
	term_member(El, Sub).
term_member(N, El, Term) :-
	NN is N - 1,
	term_member(NN, El, Term).

%	feedback(+Term)
%	Only print if verbose is asserted (basically debugging).

feedback(Term) :-
	(   verbose
	->  pce_info(Term)
	;   true
	).


		/********************************
		*         TERM EXPANSION	*
		********************************/

:- multifile
	user:term_expansion/2.
:- dynamic
	user:term_expansion/2.

user:term_expansion(A, B) :-
	pce_expansion:pce_term_expansion(A, B).

:- pop_compile_operators.
