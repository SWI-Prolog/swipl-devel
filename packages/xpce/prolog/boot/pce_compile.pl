/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.

    Last modified:
	Fri Sep 30 16:03:19 1994	Added method-group support
*/


:- module(pce_compile,
	[ pce_begin_class/2
	, pce_begin_class/3
        , pce_extend_class/1
	, pce_end_class/0
	, pce_term_expansion/2
	, pce_compiling/1
	, pce_group/1
	, default/3
	]).

:- meta_predicate
      pce_begin_class(:, +),
      pce_begin_class(:, +, +),
      pce_extend_class(:).


:- use_module(pce_principal).
:- use_module(pce_operator).
pce_ifhostproperty(prolog(quintus),
		   (:- use_module(pce_utils))).
:- require([ between/3
	   , concat/3
	   , concat_atom/2
	   , forall/2
	   , genarg/3
	   ]).


:- dynamic
	compiling/1,
	load_module/1,
	current_group_/1,
	verbose/0.


%	Function that allows you to send messages to the currently compiling
%	class.  Should be made a var pushed/popped by pce_begin_class and
%	pce_end_class.

:- initialization
	new(@class, var(class, class, @nil)).

		/********************************
		*           BEGIN CLASS		*
		********************************/


%	pce_begin_class(+TermDef, +Super[, Documentation])
%	Start definition of a class whose name is the functor of `TermDef'
%	and whose object/2 arguments are created using the selectors from
%	the arguments of `TermDef',  `Super' is the name of the superclass

pce_begin_class(TermDef, Super) :-
	pce_begin_class(TermDef, Super, "").

pce_begin_class(TermDef, Super, Doc) :-
	strip_module(TermDef, Module, TD),
	asserta(pce_compile:load_module(Module)),
	pce_begin_class_(TD, Super, Doc).
	

pce_begin_class_(TermDef, Super, Doc) :-
	TermDef =.. [ClassName|TermArgs],
	class_name(Class, ClassName),
	object(Class), !,			% redefined existing class
	(   get(Class, creator, built_in)
	->  pce_error(builtin_class_not_redefined(ClassName)),
	    fail
	;   true
	),
	get(Class, super_class, SuperClass),
	(   (   Super == @nil,
		SuperClass == @nil
	    ;   SuperClass \== @nil,
		class_name(SuperClass, Super)
	    )
	->  true
        ;   pce_error(superclass_not_changed(ClassName))
    	),
	term_names(Class, TermArgs),
	set_source_location(Class),
	get(Class, delegate, Delegates),
	send(Delegates, clear),
	send(Delegates, merge, SuperClass?delegate),
	feedback(reloading_class(ClassName)),
	start_class(ClassName, Doc).
pce_begin_class_(TermDef, SuperName, Doc) :-
	TermDef =.. [ClassName|TermArgs],
	source_location_term(Source),
	(   get(@pce, convert, SuperName, class, Super)
	->  true
	;   pce_error(superclass_not_exist(SuperName, ClassName))
	),
	get(Super, class_name, ClassClass),
	ClassTerm =.. [ClassClass, ClassName, Super],
	new(Class, ClassTerm),
	term_names(Class, TermArgs),
	send(Class, source, Source),
	feedback(loading_class(ClassName)),
	start_class(ClassName, Doc).


source_location_term(source_location(File, Line)) :-
	source_location(File, Line), !.
source_location_term(@nil).

set_source_location(Obj) :-
	source_location_term(Loc),
	send(Obj, source, Loc).


term_names(_, []) :- !.
term_names(Class, Selectors) :-
	VectorTerm =.. [vector|Selectors],
	send(Class, term_names, new(VectorTerm)).

pce_extend_class(ClassName) :-
	strip_module(ClassName, Module, CN),
	asserta(pce_compile:load_module(Module)),
	class_name(Class, CN),
	object(Class),
	feedback(extending_class(ClassName)),
	start_class(CN).


%	start_class(+ClassName[, +Doc])
%	Install term-expansion predicate and operators to perform the actual
%	loading of the class.

start_class(ClassName, "") :- !,
	start_class(ClassName).
start_class(ClassName, Doc) :-
	class_name(Class, ClassName),
	send(Class, summary, string(Doc)),
	start_class(ClassName).

start_class(ClassName) :-
	class_name(Class, ClassName),
	send(@class, assign, Class, global),
	asserta(compiling(ClassName)),
	asserta(current_group_(@default)),
	push_compile_operators.

%	pce_end_class.
%	Restore the old environment for term_expansion and operators.

pce_end_class :-
	(   compiling(ClassName)
	->  retract(load_module(_)),
	    retractall(compiling(ClassName)),
	    ignore(retractall(current_group_(_))),
	    (   compiling(Outer)
	    ->  class_name(OuterClass, Outer),
		send(@class, assign, OuterClass, global)
	    ;   true
	    ),
	    pop_compile_operators,
	    feedback(loaded_class(ClassName))
	;   pce_error(context_error((:- pce_end_class), noclass, declaration))
	).

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
	

%	current_class(-Class)
%	Class for which we are currently compiling.

current_class(Class) :-
	compiling(ClassName),
	class_name(Class, ClassName).

%	pce_compiling(-ClassName)
%	External function to get the current classname

pce_compiling(ClassName) :-
	compiling(ClassName).

		 /*******************************
		 *	  METHOD GROUPS		*
		 *******************************/

%	pce_group(+GroupName)
%	Define following methods to be in this group (documentation)

pce_group(GroupName) :-
	(   atom(GroupName)
	;   GroupName == @default
	), !,
	ignore(retract(current_group_(_))),
	asserta(current_group_(GroupName)).
pce_group(GroupName) :-
	pce_error(type_error((:- pce_group(GroupName)),1,atom,GroupName)).

current_group(GroupName) :-
	current_group_(GroupName), !.
current_group(@default).		% should not happen


		/********************************
		*            EXPAND		*
		********************************/

:- push_compile_operators.

pce_term_expansion(Term, Expanded) :-
	class_term(Term),
	compiling(_), !,
	(   do_expand(Term, Expanded)
	->  true
	;   pce_error(expand_failed(Term)),
	    Expanded = []
	).
pce_term_expansion(Term, _) :-
	compiling(_),
	(   Term = (_Head :- [A|B] :: _Body),
	    is_string([A|B])
	;   Term = (Head :- _Body),
	    typed_head(Head)
	),
	pce_error(context_error(Term, nomethod, clause)),
	fail.
	
is_string([]).
is_string([H|T]) :-
	between(0, 255, H),
	is_string(T).

typed_head(T) :-
	functor(T, _, Arity),
	Arity > 1,
	forall(genarg(N, T, A),
	       (   N > 1
	       ->  nonvar(A),
		   A = TP:_,
		   ground(TP)
	       ;   true
	       )).

class_term(variable(_Name, _Type, _Access)).
class_term(variable(_Name, _Type, _Access, _Doc)).
class_term(resource(_Name, _Type, _Default)).
class_term(resource(_Name, _Type, _Default, _Doc)).
class_term(delegate_to(_VarName)).
class_term(handle(_X, _Y, _Kind)).
class_term(handle(_X, _Y, _Kind, _Name)).
class_term((_Head :-> _Body)).
class_term((_Head :<- _Body)).


		/********************************
		*           DO_EXPAND		*
		********************************/

do_doc(List, string(List)) :-
	List = [_|_], !.
do_doc(Rest, Rest).

do_expand(variable(Name, Type, Acs), Expanded) :-
	do_expand(variable(Name, Type, Acs, @default), Expanded).
do_expand(variable(Name, Type := Initial, Acs, Doc),
	(?- send(Class, instance_variable,
		 new(Var, variable(Name, PceType, Acs, TheDoc))),
	    send(Var, initial_value, Initial))) :-
	do_doc(Doc, TheDoc),
	access(Acs),
	current_class(Class),
	type(Type, PceType).
do_expand(variable(Name, Type, Acs, Doc),
	(?- send(Class, instance_variable,
		 variable(Name, PceType, Acs, TheDoc, Group)))) :- !,
	do_doc(Doc, TheDoc),
	access(Acs),
	current_class(Class),
	current_group(Group),
	type(Type, PceType).

do_expand(resource(Name, Type, Def),
	(?- send(Class, resource, resource(Name, @default, PceType, PceDef,
					  Class)))) :- !,
	current_class(Class),
	to_atom(Def, PceDef),
	type(Type, PceType).
do_expand(resource(Name, Type, Def, Doc),
	(?- send(Class, resource, resource(Name, @default, PceType, PceDef,
					  Class, string(Doc))))) :- !,
	current_class(Class),
	to_atom(Def, PceDef),
	type(Type, PceType).

do_expand(handle(X, Y, Kind),
	(?- send(Class, handle, handle(X, Y, Kind)))) :- !,
	current_class(Class).

do_expand(handle(X, Y, Kind, Name),
	(?- send(Class, handle, handle(X, Y, Kind, Name)))) :- !,
	current_class(Class).

do_expand(delegate_to(VarName),
	(?- send(Class, delegate, VarName))) :- !,
	current_class(Class).

do_expand((Head :-> DocBody),			% Prolog send
	[ (?- send(Class, send_method,
		  send_method(Selector, Types, Cascade, Doc, Loc, Group)))
	, (PlHead :- Body)
	]) :- !,
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	current_class(Class),
	current_group(Group),
	class_name(Class, ClassName),
	prolog_head(send, Head, Selector, Types, PlHead, Cascade),
	feedback(expand_send(ClassName, Selector)).


do_expand((Head :<- DocBody),			% Prolog get
	[ (?- send(Class, get_method,
		   get_method(Selector, RType, Types,
			      Cascade, Doc, Loc, Group)))
	, (PlHead :- Body)
	]) :- !,
	extract_documentation(DocBody, Doc, Body),
	source_location_term(Loc),
	current_class(Class),
	current_group(Group),
	class_name(Class, ClassName),
	return_type(Head, RType),
	prolog_head(get, Head, Selector, Types, PlHead, Cascade),
	feedback(expand_get(ClassName, Selector)).

extract_documentation((DocText::Body), string(DocText), Body) :- !.
extract_documentation((DocText,Body), string(DocText), Body) :-
	is_string(DocText), !,
	pce_error(summary_not_closed(DocText)).
extract_documentation(Body, @default, Body).


return_type(Term, RType) :-
	functor(Term, _, Arity),
	arg(Arity, Term, Last),
	(   nonvar(Last),
	    Last = _:Type
	->  type(Type, RType)
	;   RType = @default
	).


prolog_head(SendGet, Head, Selector, TypeVector, PlHead, Cascade) :-
	Head =.. [Selector, Receiver | Args],
	predicate_name(SendGet, Selector, PredName),
	pl_head_args(SendGet, Args, Types, PlArgs, FArgs),
	create_type_vector(Types, TypeVector),
	PlHead =.. [PredName, Receiver | PlArgs],
	(   SendGet == send
	->  Class = message
	;   Class = ?
	),
	Cascade =.. [Class, @prolog, call, PredName, @receiver | FArgs].

create_type_vector([],      @default) :- !.
create_type_vector(List,    new(VectorTerm)) :-
	VectorTerm =.. [vector|List].


predicate_name(SendGet, Selector, Name) :-
	current_class(Class),
	class_name(Class, ClassName),
	concat_atom([SendGet, '_', Selector, '_', ClassName], Name).


pl_head_args(SendGet, Args, Types, PlArgs, FArgs) :-
	pl_head_args(SendGet, Args, 1, Types, PlArgs, FArgs).
	
pl_head_args(send, [], _, [], [], []) :- !.
pl_head_args(get,  [Return], _, [], [ReturnVar], []) :- !,
	(   var(Return)
	->  ReturnVar = Return
	;   Return = ReturnVar:_Type
	).
pl_head_args(SG, [ArgAndType|RA], AN, [T|RT], [Arg|TA], [@ArgN|MArgs]) :- !,
	head_arg(ArgAndType, Arg, Type),
	type(Type, T),
	concat(arg, AN, ArgN),
	ANN is AN + 1,
	pl_head_args(SG, RA, ANN, RT, TA, MArgs).


head_arg(Var, Var, any) :-
	var(Var), !.
head_arg(Arg:Type, Arg, Type).

%	class_name(?Class, ?ClassName)
%	Convert between PCE class-name and PCE Class object

class_name(Class, ClassName) :-
	object(Class), !,
	get(Class, name, ClassName).
class_name(Class, ClassName) :-
	atom(ClassName), !,
	get(@classes, member, ClassName, Class).
class_name(Class, ClassName) :-
	pce_error(instantiation_error(class_name(Class, ClassName))),
	fail.

%	access(?Access)
%	List of legal access names.

access(none).
access(get).
access(send).
access(both).

%	type(+Spec, -PceTypeName)
%	Convert type specification into legal PCE type-name

type(Prolog, Pce) :-
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
	term_to_atom(Term, RawAtom),
	new(S, string(RawAtom)),
	send(S, translate, 0' , @nil),
	get(S, value, Atom).
to_atom(Term, any) :-
	pce_error(type_error(to_atom(Term, any), 1, ground, Term)).


:- pop_compile_operators.

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

:- initialization(user:assert((term_expansion(A, B) :-
			      pce_compile:pce_term_expansion(A, B)))).


		/********************************
		*             DEFAULTS		*
		********************************/

%	default(+Argument, +Default, -Value)
%	default(+Argument, resource(+Object, +Name), -Value)
%
%	Get the default value for an argument.

default(@default, resource(Obj, Name), Value) :- !, 
	(   get(Obj, resource_value, Name, Value)
	->  true
	;   pce_error(get_resource_failed(Name, Obj)),
	    fail
	).
default(@default, Default, Default) :- !.
default(Value,    _Default, Value).
