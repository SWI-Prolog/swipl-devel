/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/


:- module(man_util,
	[ has_attribute/2			  % +Obj x +Name
	, has_relation/2			  % +Obj x +Name
	, has_attribute_value/2			  % +Obj x +Name
	, has_relation_value/2			  % +Obj x +Name
	, man_attribute/1
	, man_relation/1
	, apropos_class/6
	, class_of/2
	, concat_atom/3
	, ifmaintainer/1
	, group_objects/2
	, indent/2
	, man_classification/2
	, super_or_delegate_class/2	% +Class, -SuperOrDelegate
	, class_of_type/2		% +Type, -Class
	]).

:- meta_predicate
	ifmaintainer(:).

:- use_module(library(pce)).
:- require([ chain_list/2
	   , concat_atom/2
	   , forall/2
	   , get_chain/3
	   , member/2
	   ]).


		 /*******************************
		 *	 CLASS EXTENSIONS	*
		 *******************************/

:- pce_extend_class(object).

:- pce_global(@man_description_cache,	new(hash_table)).
:- pce_global(@man_source_cache,	new(hash_table)).
:- pce_global(@man_tab,			new(string('\t'))).
:- pce_global(@man_nl,			new(string('\n'))).
:- pce_global(@man_indent,		new(string('\n\t'))).
:- pce_global(@man_nl_regex,		new(regex(string('\n')))).
:- pce_global(@man_indent_message,
	      new(message(@arg1, replace, @arg2, @man_indent))).

man_header(Object, Header) :<-
	get(Object, man_name, Header).

group(Object, Group:name) :<-
	"Classes are of the group class"::
	get(Object, class_name, Group).

indent(T, S) :-
	new(S, string('\t%s', T)),
	send(@man_nl_regex, for_all, S, @man_indent_message).


man_description(Obj, Descr:char_array) :<-
	(   get(@man_description_cache, member, Obj, Descr)
	->  true
	;   (	get(Obj, man_attribute, description, D0)
	    ;	get(Obj, man_inherited_attribute, description,
		    tuple(From, D0))
	    ;	get(Obj, summary, D0)
	    ;	D0 = '(not documented)'
	    ),
	    D0 \== @nil
	->  indent(D0, Descr),
	    send(@man_description_cache, append, Obj, Descr),
	    (	nonvar(From)
	    ->	send(@man_source_cache, append, Obj, From)
	    ;	true
	    )
	).


man_description_source(Obj, Source:object) :<-
	"Object that provided the description"::
	(   get(@man_source_cache, member, Obj, Source)
	->  true
	;   Source = Obj
	).

:- pce_end_class.


		 /*******************************
		 *	UTILITY PREDICATES	*
		 *******************************/

%	man_attribute(?Attribute).
%	Is true if Attribute is the name of a manual card attribute

man_attribute(description).
man_attribute(diagnostics).
man_attribute(defaults).
man_attribute(user_interface).
man_attribute(code).
man_attribute(bugs).

%	man_relation(?Relation)
%	Is true if Relation is the name of a manual card relation

man_relation(see_also).

%	has_attribute(+Object, +Selector)
%	Is true if Object has an attribute named Selector.

has_attribute(Obj, Selector) :-
	man_attribute(Selector),
	(   get(Obj?class, instance_variable, Selector, _Var)
	;   send(Obj, has_get_method, man_card_class),
	    get(Obj?man_card_class, instance_variable, Selector, _Var)
	), !.

%	has_relation(+Object, +Selector)
%	Is true if Object has a relation named Selector.

has_relation(Obj, Selector) :-
	man_relation(Selector),
	(   get(Obj?class, instance_variable, Selector, _Var)
	;   send(Obj, has_get_method, man_card_class),
	    get(Obj?man_card_class, instance_variable, Selector, _Var)
	), !.

%	has_attribute_value(+Obj, +Selector)

has_attribute_value(Obj, Selector) :-
	get(Obj, man_attribute, Selector, Val), Val \== @nil.

%	has_relation_value(+Obj, +Selector)

has_relation_value(Obj, Selector) :-
	get(Obj, man_related, Selector, Val), Val \== @nil,
	\+ send(Val, empty).


		 /*******************************
		 *    INHERITANCE/DELEGATION	*
		 *******************************/

%	super_or_delegate_class(+Class, -Super)
%
%	Successively unifies `Super' with class objects this `Class' inherits
%	from or delegates too.

:- dynamic done_class/1.
	
super_or_delegate_class(Class, Super) :-
	retractall(done_class(_)),
	super_or_delegate_class_(Class, Super).
super_or_delegate_class(_, _) :-
	retractall(done_class(_)),
	fail.

super_or_delegate_class_(Class, _) :-
	done_class(Class), !,
	fail.
super_or_delegate_class_(Class, Class) :-
	asserta(done_class(Class)).
super_or_delegate_class_(Class, Super) :-
	get(Class, super_class, ThisSuper),
	ThisSuper \== @nil,
	super_or_delegate_class_(ThisSuper, Super).
super_or_delegate_class_(Class, Delegate) :-
	get_chain(Class, delegate, List),
	member(Var, List),
	get(Var, type, Type),
	class_of_type(Type, DelClass),
	\+ done_class(DelClass),
	super_or_delegate_class_(DelClass, Delegate).

class_of_type(Type, Class) :-
	get(Type, kind, class),
	get(Type, context, Class).
class_of_type(Type, Class) :-
	get_chain(Type, supers, Supers),
	member(Super, Supers),
	class_of_type(Super, Class).


		/********************************
		*           FIND OBJECTS	*
		********************************/

%	apropos_class(+Class, +Inherit, +Types, +Fields, +Keyword, -Matches)

apropos_class(Class, Inherit, Types, Fields, Keyword, Match) :- !,
	(   Keyword == '' ; Keyword = '.*'
	->  Regex = @nil
	;   new(Regex, regex(Keyword)),
	    send(Regex, ignore_case, @off),
	    send(Regex, compile, @on)
	),
	new(Flds, chain),
	forall(member(Field, Fields),
	       (   map_field(Field, Selector),
		   send(Flds, append, Selector)
	       )),
	new(Match0, chain),
	apropos_class_(Inherit, Class, Types, Flds, Regex, Match0),
	get(Match0, find_all,
	    message(@manual, in_scope, @arg1), Match),
	send(Match0, done),
	send(Flds, done).

map_field(description, man_description) :- !.
map_field(X, X).

apropos_class_(own, Class, Types, Flds, Regex, Match) :- !,
	forall(member(Type, Types),
	       apropos_type_attribute(Type, Class, Flds, Regex, Match)).
apropos_class_(sub, Class, Types, Flds, Regex, Match) :- !,
	apropos_class_(own, Class, Types, Flds, Regex, Match),
	(   get_chain(Class, sub_classes, Subs)
	->  forall(member(Sub, Subs),
		   apropos_class_(sub, Sub, Types, Flds, Regex, Match))
	;   true
	).
apropos_class_(super, Class, Types, Flds, Regex, Match) :- !,
	apropos_class_(@object_class, Class, Types, Flds, Regex, Match).

apropos_class_(Scope, Class, Types, Flds, Regex, Match) :-
	forall((super_or_delegate_class(Class, Super),
		send(Scope, member, Super)),
	       apropos_class_(own, Super, Types, Flds, Regex, Match)),
	cleanup(Match).

cleanup(Match) :-
	new(Done, chain_table),
	new(Tmp, chain),

	new(AppendMatch, message(Tmp, append, @arg1)),
	new(Selector, @arg1?name),
	new(DoneSelector, ?(Done, member, Selector)),

	send(Match, for_all,
	     if(message(@arg1, instance_of, send_method),
		if(not(message(DoneSelector, member, send)),
		   and(AppendMatch,
		       message(Done, append, Selector, send))),
		if(message(@arg1, instance_of, get_method),
		   if(not(message(DoneSelector, member, get)),
		   and(AppendMatch,
		       message(Done, append, Selector, get))),
		   AppendMatch)),
	     @off),
	send(Match, clear),
	send(Match, merge, Tmp),
	send(Done, done).

apropos_type_attribute(self, Class, Fields, Keyword, Match) :- !,
	(   match_apropos(Class, Fields, Keyword)
	->  send(Match, append, Class)
	;   true
	).
apropos_type_attribute(variable, Class, Fields, Keyword, Matches) :- !,
	get(Class, instance_variables, Vars),
	new(Locals, chain),
	send(Vars, for_all, if(@arg1?context == Class,
			       message(Locals, append, @arg1))),
	chain_list(Locals, List),
	send(Locals, done),
	(   member(Object, List),
	    match_apropos(Object, Fields, Keyword),
	    send(Matches, append, Object),
	    fail
	;   true
	).
apropos_type_attribute(Att, Class, Flds, Regex, Match) :-
	type_to_class_attribute(Att, PT),
	apropos_class_attribute(Class, PT, Flds, Regex, Match).

type_to_class_attribute(send_method, send_methods) :- !.
type_to_class_attribute(get_method,  get_methods) :- !.
type_to_class_attribute(resource,    resources) :- !.
type_to_class_attribute(sub_class,   sub_classes).

%	apropos_class_attribute(+Class, +Att, +Flds, +Kwd, +Match)
%	Append matching objects of Class to Match

apropos_class_attribute(Class, Att, _Fields, @nil, Matches) :- !,
	get(Class, Att, Chain),
	send(Matches, merge, Chain).
apropos_class_attribute(Class, Att, Fields, Regex, Matches) :-
	get(Class, Att, Chain),
	pce_catch_error(argument_type,
			send(Chain, for_all,
			     and(assign(new(Candidate, var), @arg1),
				 if(?(Fields, find,
				      message(Regex, search,
					      Candidate ? @arg1)),
				    message(Matches, append, Candidate))))).

%	match_apropos(+Object, +Fields, +Regex)
%	Test if Object contains Regex in one of the specified fields

match_apropos(_Object, _Fields, @nil) :- !.
match_apropos(Object, Fields, Regex) :-
	pce_catch_error(argument_type,
			get(Fields, find,
			    message(Regex, search, Object ? @arg1), _)).


%	class_of(+Object, -ClassName)
%	When Object is related to a class, return the classname.  Otherwise
%	return the atom ''.

class_of(Class, ClassName) :-
	send(Class, instance_of, class), !,
	get(Class, name, ClassName).
class_of(Obj, ClassName) :-
	send(Obj, instance_of, behaviour),
	get(Obj, context, Context),
	send(Context, instance_of, class), !,
	get(Context, name, ClassName).
class_of(Obj, ClassName) :-
	send(Obj, instance_of, resource),
	get(Obj, context, Context),
	send(Context, instance_of, class), !,
	get(Context, name, ClassName).
class_of(_, '').


		 /*******************************
		 *	     GROUPING		*
		 *******************************/

%	Translate a chain of objects into a sheet of groups

group_objects(Chain, Groups) :-
	new(Groups, sheet),
	Group = when(@arg1?group, @arg1?group, miscellaneous),
	send(Chain, for_all,
	     if(message(Groups, is_attribute, Group),
		message(?(Groups, value, Group), append, @arg1),
		message(Groups, value, Group,
			?(@pce, instance, chain, @arg1)))),

	SortByName = ?(@arg1?name, compare, @arg2?name),

	order_groups(Groups),		% TBD
%	send(Groups?members, sort, ?(@arg1?name, compare, @arg2?name)),

	send(Groups?members, for_all,
	     message(@arg1?value, sort,
		     quote_function(SortByName))).

%	order_groups(Groups)
%	Order chain of groups

order_groups(Sheet) :-
	get(@manual, module, groups, @on, GroupModule),
	get(GroupModule, id_table, Table),
	get(Sheet, members, Chain),
	new(Unordered, chain),
	send(Chain, for_all,
	     if(not(?(Table, member, @arg1?name)),
		and(message(Unordered, append, @arg1),
		    message(Chain, delete, @arg1)))),
	send(Chain, sort,
	     ?(?(Table, member, @arg1?name)?index, compare,
	       ?(Table, member, @arg2?name)?index)),
	send(Chain, merge, Unordered).


		/********************************
		*             CONCAT		*
		********************************/

%	concat_atom(+ListOfAtoms, +Separator, -Result).

concat_atom([], _, '').
concat_atom([X], _, X).
concat_atom([X|Y], Sep, Result) :-
	concat_atom(Y, Sep, R0),
	concat_atom([X, Sep, R0], Result).


		/********************************
		*            MAINTAINER		*
		********************************/

ifmaintainer(Goal) :-
	get(@manual, maintainer, @on), !,
	Goal.
ifmaintainer(_).


		 /*******************************
		 *	  CLASSIFICATION	*
		 *******************************/

man_classification(basic,			'Basic').
man_classification(advanced,			'Advanced').
man_classification(obscure,			'Rare').
man_classification(internal,			'Internal').
man_classification(basic_programming,		'Basic OO').
man_classification(advanced_programming,	'Advanced OO').
man_classification(user,			'Application').
