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
:- require([ between/3
	   , concat/3
	   , concat_atom/2
	   , delete/3
	   , append/3
	   , flatten/2
	   , forall/2
	   , genarg/3
	   , is_list/1
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
	recording/2,			% items recorded
	dynamic_declared/2.		% +Head, +Path

pce_ifhostproperty(prolog(swi), (:- index(attribute(1,1,0)))).


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
	(   is_list(In0)
	->  maplist(map_term_expand, In0, In1),
	    flatten(In1, Out0),
	    (	Out0 = [X]
	    ->	Out = X
	    ;	Out = Out0
	    )
	;   do_term_expand(In0, Out)
	).

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
do_pce_pre_expand(resource(Name, Type, Default),
		  resource(Name, Type, Default, @default)) :-
	pce_compiling.
do_pce_pre_expand(handle(X, Y, Kind),
		  handle(X, Y, Kind, @default)) :-
	pce_compiling.
do_pce_pre_expand((:- use_class_template(Template)),
		  (:- pce_class_directive(use_class_template(Template)))).
do_pce_pre_expand((:- ClassDirective), D) :-
	functor(ClassDirective, send, _),
	arg(1, ClassDirective, @class), !,
	D = (:- pce_class_directive(ClassDirective)).
do_pce_pre_expand(X, X).


%	pce_post_expand(In, Out)

pce_post_expand(X, X).

%	pce_expandable(+Term)
%	Quick test whether we can expand this.

pce_expandable((:- pce_begin_class(_Class, _Super, _Doc))).
pce_expandable((:- pce_extend_class(_Class))).
pce_expandable((:- pce_end_class)).
pce_expandable((:- pce_group(_))).
pce_expandable((:- pce_class_directive(_))).
pce_expandable(variable(_Name, _Type, _Access, _Doc)) :-
	pce_compiling.
pce_expandable(resource(_Name, _Type, _Default, _Doc)) :-
	pce_compiling.
pce_expandable(delegate_to(_VarName)) :-
	pce_compiling.
pce_expandable(handle(_X, _Y, _Kind, _Name)) :-
	pce_compiling.
pce_expandable((_Head :-> _Body)).
pce_expandable((_Head :<- _Body)).
pce_expandable(end_of_file).


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
	set_attribute(ClassName, extending, true),
	prolog_load_context(file, SourceFile),
	file_base_name(SourceFile, SourceTag),
	set_attribute(ClassName, method_tag, SourceTag).
do_expand((:- pce_end_class), Result) :-
	pce_compiling(ClassName),
	findall(M, retract(attribute(ClassName, send_method, M)), SMS),
	findall(M, retract(attribute(ClassName, get_method,  M)), SGS),
	dynamic_declaration(SMS, Decl1),
	dynamic_declaration(SGS, Decl2),
	method_clauses(ClassName, Clauses),
	(   attribute(ClassName, extending, true)
	->  dynamic_declaration([class(_,_,_,_,_,_)], ClassDecl),
	    term_expand((:- initialization(pce_extended_class(ClassName))),
			ExtendDecl),
	    flatten([ Clauses,
		      Decl1, SMS,
		      Decl2, SGS,
		      ClassDecl,
		      ExtendDecl
		    ], Result)
	;   retract(attribute(ClassName, super, Super)),
	    retract(attribute(ClassName, meta, MetaClass)),
	    findall(V, retract(attribute(ClassName, variable, V)),  Variables),
	    findall(R, retract(attribute(ClassName, resource, R)),  Resources),
	    findall(D, retract(attribute(ClassName, directive, D)), Directs),
	    ClassFact = class(ClassName, MetaClass, Super,
			      Variables,
			      Resources,
			      Directs),
	    dynamic_declaration([ClassFact], ClassDecl),
	    term_expand((:- initialization(pce_register_class(ClassName))),
			RegisterDecl),
	    flatten([ Clauses,
		      Decl1,	 SMS,
		      Decl2,	 SGS,
		      ClassDecl, ClassFact,
		      RegisterDecl
		    ], Result)
	),
	pop_class.
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
do_expand(resource(Name, Type, Default, Doc), []) :-
	pce_compiling(ClassName),
	pce_type(Type, PceType),
	pce_default(Default, PceDefault),
	pce_summary(Doc, PceDoc),
	add_attribute(ClassName, resource,
		      resource(Name, PceType, PceDefault, PceDoc)).
do_expand(handle(X, Y, Kind, Name), []) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, directive,
		      send(@class, handle, handle(X, Y, Kind, Name))).
do_expand(delegate_to(Var), []) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, directive,
		      send(@class, delegate, Var)).
do_expand((:- pce_class_directive(Goal)), [(:- Goal)]) :-
	pce_compiling(ClassName),
	get(@classes, member, ClassName, C),
	get(C, realised, @on), !.
do_expand((:- pce_class_directive(Goal)), []) :-
	pce_compiling(ClassName),
	add_attribute(ClassName, directive, Goal).
do_expand((Head :-> DocBody), []) :-
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	pce_compiling(ClassName),
	current_group(ClassName, Group),
	prolog_head(send, Head, Selector, Types, PlHead),
	functor(PlHead, PredName, _),
	strip_defaults([Group, Loc, Doc], NonDefArgs),
	LSM =.. [bind_send_method, PredName, Types | NonDefArgs],
	add_attribute(ClassName, method_clause, (PlHead :- Body)),
	add_attribute(ClassName, send_method,
		      lazy_send_method(Selector, ClassName, LSM)),
	feedback(expand_send(ClassName, Selector)).
do_expand((Head :<- DocBody), []) :-
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	pce_compiling(ClassName),
	current_group(ClassName, Group),
	return_type(Head, RType),
	prolog_head(get, Head, Selector, Types, PlHead),
	functor(PlHead, PredName, _),
	strip_defaults([Group, Loc, Doc], NonDefArgs),
	LGM =.. [bind_get_method, RType, PredName, Types | NonDefArgs],
	add_attribute(ClassName, method_clause, (PlHead :- Body)),
	add_attribute(ClassName, get_method,
		      lazy_get_method(Selector, ClassName, LGM)),
	feedback(expand_get(ClassName, Selector)).
do_expand(end_of_file, end_of_file) :-
	prolog_load_context(file, Path),
	retractall(dynamic_declared(_, Path)).

term_expand(T0, T) :-
	user:term_expansion(T0, T), !.
term_expand(T0, T0).

%	method_clauses(+ClassName, -Clauses)
%
%	Collect the method_clause attributes and sort them such that
%	discontiguous declarations are not necessary and declarations
%	may thus be minimised.

method_clauses(ClassName, Clauses) :-
	findall(C, retract(attribute(ClassName, method_clause, C)), C0),
	sort(C0, C1),
	make_extern(C1, Clauses).

pce_ifhostproperty(need_extern_declaration,
(make_extern(Clauses, ExternClauses) :-
	make_extern_decls(Clauses, Extern),
	append(Extern, Clauses, ExternClauses)),
make_extern(Clauses, Clauses)).

make_extern_decls([], []).
make_extern_decls([H|T], [E0|E]) :-
	predicate_name_and_arity(H, NA),
	extern_decl(NA, E0),
	delete_other_clauses(T, NA, T1),
	make_extern_decls(T1, E).

delete_other_clauses([H|T], NA, L) :-
	predicate_name_and_arity(H, NA), !,
	delete_other_clauses(T, NA, L).
delete_other_clauses(L, _, L).
	
predicate_name_and_arity((H:-_), N/A) :- !,
	functor(H, N, A).
predicate_name_and_arity(H, N/A) :-
	functor(H, N, A).
	
extern_decl(Name/Arity, (:-extern(EH))) :-
	functor(EH, Name, Arity),
	extern_arg(0, Arity, EH).

extern_arg(N, N, _) :- !.
extern_arg(N, A, T) :-
	NN is N + 1,
	arg(NN, T, +term),
	extern_arg(NN, A, T).
	

strip_defaults([@default|T0], T) :- !,
	strip_defaults(T0, T).
strip_defaults(L, LV) :-
	reverse(L, LV).

break_class_specification(Meta:Term, ClassName, Meta, TermArgs) :- !,
	Term =.. [ClassName|TermArgs].
break_class_specification(Term, ClassName, @default, TermArgs) :-
	Term =.. [ClassName|TermArgs].


		 /*******************************
		 *	   DECLARATIONS		*
		 *******************************/

dynamic_declaration([], []).
dynamic_declaration([Head|_], []) :-
	functor(Head, Name, Arity),
	declared(Name, Arity), !.
pce_ifhostproperty(no_discontiguous,
(   dynamic_declaration([Head|_],
		    [ (:- multifile(Name/Arity)),
		      (:- dynamic(Name/Arity))
		    ]) :-
	functor(Head, Name, Arity)),
(   dynamic_declaration([Head|_],
		    [ (:- multifile(Name/Arity)),
		      (:- dynamic(Name/Arity)),
		      (:- discontiguous(Name/Arity))
		    ]) :-
	functor(Head, Name, Arity))).

declared(Name, Arity) :-
	functor(Head, Name, Arity),
	prolog_load_context(file, Path),
	(   dynamic_declared(Head, Path)
	->  true
	;   assert(dynamic_declared(Head, Path)),
	    fail
	).
		   

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
	(   get(@classes, member, ClassName, Class),
	    get(Class, realised, @on)
	->  get(@class, '_value', OldClassVal),
	    asserta(attribute(ClassName, old_class_val, OldClassVal)),
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
		 *	       RESOURCE		*
		 *******************************/

pce_default(Term, Term) :-
	atomic(Term), !.
pce_default(Term, PceTerm) :-
	term_to_atom(Term, PceTerm).

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


prolog_head(SendGet, Head, Selector, TypeVector, PlHead) :-
	Head =.. [Selector, Receiver | Args],
	predicate_name(SendGet, Selector, PredName),
	pl_head_args(SendGet, Args, Types, PlArgs),
	create_type_vector(Types, TypeVector),
	PlHead =.. [PredName, Selector, Receiver | PlArgs].


create_type_vector([],      @default) :- !.
create_type_vector(List,    VectorTerm) :-
	VectorTerm =.. [vector|List].

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Predicates implementing methods are  normally called {send,get}_<class>,
which is fine for :-   pce_begin_class/:- pce_end_class defined classes.
Class  extension  however,  could  force   multiple  occurences  of  the
predicate. To avoid this, the predicate is tagged with the *basename* of
the sourcefile. Older versions used  the   full  pathname,  but symbolic
links can cause trouble reloading in this  case. The current schema only
fails if a class is defined  in   multiple  files with the same basename
that are not module files.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

predicate_name(SendGet, _Selector, Name) :-
	pce_compiling(ClassName),
	(   attribute(ClassName, method_tag, Tag)
	->  concat_atom([SendGet, '_', ClassName, '_', Tag], Name)
	;   concat_atom([SendGet, '_', ClassName], Name)
	).


pl_head_args(send, [], [], []) :- !.
pl_head_args(get,  [Return], [], [ReturnVar]) :- !,
	(   var(Return)
	->  ReturnVar = Return
	;   Return = ReturnVar:_Type
	).
pl_head_args(SG, [ArgAndType|RA], [T|RT], [Arg|TA]) :- !,
	head_arg(ArgAndType, Arg, Type),
	pce_type(Type, T),
	pl_head_args(SG, RA, RT, TA).


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
