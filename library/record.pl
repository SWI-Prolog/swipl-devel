/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2007, University of Amsterdam

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

:- module((record),
	  [ (record)/1,			% +Record
	    current_record/2,		% ?Name, ?Term
	    current_record_predicate/2,	% ?Record, :PI
	    op(1150, fx, record)
	  ]).
:- use_module(library(error)).

/** <module> Access compound arguments by name

This module creates a set of predicates   to  create a default instance,
access and modify records represented as a compound term.

The full documentation is  with  record/1,  which   must  be  used  as a
_directive_.  Here is a simple example declaration and some calls.

==
:- record point(x:integer=0, y:integer=0).

	default_point(Point),
	point_x(Point, X),
	set_x_of_point(10, Point, Point1),

	make_point([y(20)], YPoint),
==

@author Jan Wielemaker
@author Richard O'Keefe
*/

:- multifile
	error:has_type/2,
	prolog:generated_predicate/1.

error:has_type(record(M:Name), X) :-
	current_record(Name, M, _, X, IsX), !,
	call(M:IsX).

%%	record(+RecordDef)
%
%	Define access predicates for a compound-term. RecordDef is of
%	the form <constructor>(<argument>, ...), where each argument
%	is of the form:
%
%	  * <name>[:<type>][=<default>]
%
%	Used a directive, =|:- record Constructor(Arg, ...)|= is expanded
%	info the following predicates:
%
%	  * <constructor>_<name>(Record, Value)
%	  * <constructor>_data(?Name, ?Record, ?Value)
%	  * default_<constructor>(-Record)
%	  * is_<constructor>(@Term)
%	  * make_<constructor>(+Fields, -Record)
%	  * make_<constructor>(+Fields, -Record, -RestFields)
%	  * set_<name>_of_<constructor>(+Value, +OldRecord, -New)
%	  * set_<name>_of_<constructor>(+Value, !Record)
%	  * nb_set_<name>_of_<constructor>(+Value, !Record)
%	  * set_<constructor>_fields(+Fields, +Record0, -Record).
%	  * set_<constructor>_fields(+Fields, +Record0, -Record, -RestFields).
%	  * set_<constructor>_field(+Field, +Record0, -Record).
%	  * user:current_record(:<constructor>)

record(Record) :-
	throw(error(context_error(nodirective, record(Record)), _)).


%%	compile_records(+RecordsDefs, -Clauses) is det.
%
%	Compile a record specification into a list of clauses.

compile_records(Spec, Clauses) :-
	phrase(compile_records(Spec), Clauses).
%	maplist(portray_clause, Clauses).

compile_records(Var) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
compile_records((A,B)) -->
	compile_record(A),
	compile_records(B).
compile_records(A) -->
	compile_record(A).

%%	compile_record(+Record)// is det.
%
%	Create clauses for Record.

compile_record(RecordDef) -->
	{ RecordDef =.. [Constructor|Args],
	  defaults(Args, Defs, TypedArgs),
	  types(TypedArgs, Names, Types),
	  atom_concat(default_, Constructor, DefName),
	  atom_concat(Constructor, '_data', DataName),
	  DefRecord =.. [Constructor|Defs],
	  DefClause =.. [DefName,DefRecord],
	  length(Names, Arity)
	},
	[ DefClause ],
	access_predicates(Names, 1, Arity, Constructor),
	data_predicate(Names, 1, Arity, Constructor, DataName),
	set_predicates(Names, 1, Arity, Types, Constructor),
	set_field_predicates(Names, 1, Arity, Types, Constructor),
	make_predicate(Constructor),
	is_predicate(Constructor, Types),
	current_clause(RecordDef).

:- meta_predicate
	current_record(?, :),
	current_record_predicate(?, :).
:- multifile
	current_record/5.		% Name, Module, Term, X, IsX

%%	current_record(?Name, :Term)
%
%	True if Name is the  name  of   a  record  defined in the module
%	associated with Term  and  Term   is  the  user-provided  record
%	declaration.

current_record(Name, M:Term) :-
	current_record(Name, M, Term, _, _).

current_clause(RecordDef) -->
	{ prolog_load_context(module, M),
	  functor(RecordDef, Name, _),
	  atom_concat(is_, Name, IsName),
	  IsX =.. [IsName, X]
	},
	[ (record):current_record(Name, M, RecordDef, X, IsX)
	].


%%	current_record_predicate(?Record, ?PI) is nondet.
%
%	True if PI is the predicate indicator for an access predicate to
%	Record. This predicate is intended   to support cross-referencer
%	tools.

current_record_predicate(Record, M:PI) :-
	(   ground(PI)
	->  Det = true
	;   true
	),
	current_record(Record, M:RecordDef),
	(   general_record_pred(Record, M:PI)
	;   RecordDef =.. [_|Args],
	    defaults(Args, _Defs, TypedArgs),
	    types(TypedArgs, Names, _Types),
	    member(Field, Names),
	    field_record_pred(Record, Field, M:PI)
	),
	(   Det == true
	->  !
	;   true
	).

general_record_pred(Record, _:Name/1) :-
	atom_concat(is_, Record, Name).
general_record_pred(Record, _:Name/1) :-
	atom_concat(default_, Record, Name).
general_record_pred(Record, _:Name/A) :-
	member(A, [2,3]),
	atom_concat(make_, Record, Name).
general_record_pred(Record, _:Name/3) :-
	atom_concat(Record, '_data', Name).
general_record_pred(Record, _:Name/A) :-
	member(A, [3,4]),
	atomic_list_concat([set_, Record, '_fields'], Name).
general_record_pred(Record, _:Name/3) :-
	atomic_list_concat([set_, Record, '_field'], Name).

field_record_pred(Record, Field, _:Name/2) :-
	atomic_list_concat([Record, '_', Field], Name).
field_record_pred(Record, Field, _:Name/A) :-
	member(A, [2,3]),
	atomic_list_concat([set_, Field, '_of_', Record], Name).
field_record_pred(Record, Field, _:Name/2) :-
	atomic_list_concat([nb_set_, Field, '_of_', Record], Name).

prolog:generated_predicate(P) :-
	current_record_predicate(_, P).

%%	make_predicate(+Constructor)// is det.
%
%	Creates the make_<constructor>(+Fields, -Record) predicate. This
%	looks like this:
%
%	==
%	make_<constructor>(Fields, Record) :-
%		make_<constructor>(Fields, Record, [])
%
%	make_<constructor>(Fields, Record, RestFields) :-
%		default_<constructor>(Record0),
%		set_<constructor>_fields(Fields, Record0, Record, RestFields).
%
%	set_<constructor>_fields(Fields, Record0, Record) :-
%		set_<constructor>_fields(Fields, Record0, Record, []).
%
%	set_<constructor>_fields([], Record, Record, []).
%	set_<constructor>_fields([H|T], Record0, Record, RestFields) :-
%		(   set_<constructor>_field(H, Record0, Record1)
%		->  set_<constructor>_fields(T, Record1, Record, RestFields)
%		;   RestFields = [H|RF],
%		    set_<constructor>_fields(T, Record0, Record, RF)
%		).
%
%	set_<constructor>_field(<name1>(Value), Record0, Record).
%	...
%	==

make_predicate(Constructor) -->
	{ atomic_list_concat([make_, Constructor], MakePredName),
	  atomic_list_concat([default_, Constructor], DefPredName),
	  atomic_list_concat([set_, Constructor, '_fields'], SetFieldsName),
	  atomic_list_concat([set_, Constructor, '_field'], SetFieldName),
	  MakeHead3 =.. [MakePredName, Fields, Record],
	  MakeHead4 =.. [MakePredName, Fields, Record, []],
	  MakeClause3 = (MakeHead3 :- MakeHead4),
	  MakeHead =.. [MakePredName, Fields, Record, RestFields],
	  DefGoal  =.. [DefPredName, Record0],
	  SetGoal  =.. [SetFieldsName, Fields, Record0, Record, RestFields],
	  MakeClause = (MakeHead :- DefGoal, SetGoal),
	  SetHead3 =.. [SetFieldsName, Fields, R0, R],
	  SetHead4 =.. [SetFieldsName, Fields, R0, R, []],
	  SetClause0 = (SetHead3 :- SetHead4),
	  SetClause1 =.. [SetFieldsName, [], R, R, []],
	  SetHead2  =.. [SetFieldsName, [H|T], R0, R, RF],
	  SetGoal2a =.. [SetFieldName, H, R0, R1],
	  SetGoal2b =.. [SetFieldsName, T, R1, R, RF],
	  SetGoal2c =.. [SetFieldsName, T, R0, R, RF1],
	  SetClause2 = (SetHead2 :- (SetGoal2a -> SetGoal2b ; RF=[H|RF1], SetGoal2c))
	},
	[ MakeClause3, MakeClause, SetClause0, SetClause1, SetClause2 ].

%%	is_predicate(+Constructor, +Types)// is det.
%
%	Create a clause that tests for a given record type.

is_predicate(Constructor, Types) -->
	{ type_checks(Types, Vars, Body0),
	  clean_body(Body0, Body),
	  Term =.. [Constructor|Vars],
	  atom_concat(is_, Constructor, Name),
	  Head1 =.. [Name,Var],
	  Head2 =.. [Name,Term]
	},
	[   (Head1 :- var(Var), !, fail) ],
	(   { Body == true }
	->  [ Head2 ]
	;   [ (Head2 :- Body) ]
	).

type_checks([], [], true).
type_checks([any|T], [_|Vars], Body) :-
	type_checks(T, Vars, Body).
type_checks([Type|T], [V|Vars], (Goal, Body)) :-
	type_goal(Type, V, Goal),
	type_checks(T, Vars, Body).

%%	type_goal(+Type, +Var, -BodyTerm) is det.
%
%	Inline type checking calls.

type_goal(Type, Var, Body) :-
	defined_type(Type, Var, Body), !.
type_goal(record(Record), Var, Body) :- !,
	atom_concat(is_, Record, Pred),
	Body =.. [Pred,Var].
type_goal(Record, Var, Body) :-
	atom(Record), !,
	atom_concat(is_, Record, Pred),
	Body =.. [Pred,Var].
type_goal(Type, _, _) :-
	domain_error(type, Type).

defined_type(Type, Var, error:Body) :-
	clause(error:has_type(Type, Var), Body).


clean_body(M:(A0,B0), G) :- !,
	clean_body(M:A0, A),
	clean_body(M:B0, B),
	clean_body((A,B), G).
clean_body((A0,true), A) :- !,
	clean_body(A0, A).
clean_body((true,A0), A) :- !,
	clean_body(A0, A).
clean_body((A0,B0), (A,B)) :-
	clean_body(A0, A),
	clean_body(B0, B).
clean_body(_:A, A) :-
	predicate_property(A, built_in), !.
clean_body(A, A).


%%	access_predicates(+Names, +Idx0, +Arity, +Constructor)// is det.
%
%	Create the <constructor>_<name>(Record, Value) predicates.

access_predicates([], _, _, _) -->
	[].
access_predicates([Name|NT], I, Arity, Constructor) -->
	{ atomic_list_concat([Constructor, '_', Name], PredName),
	  functor(Record, Constructor, Arity),
	  arg(I, Record, Value),
	  Clause =.. [PredName, Record, Value],
	  I2 is I + 1
	},
	[Clause],
	access_predicates(NT, I2, Arity, Constructor).


%%	data_predicate(+Names, +Idx0, +Arity, +Constructor, +DataName)// is det.
%
%	Create the <constructor>_data(Name, Record, Value) predicate.

data_predicate([], _, _, _, _) -->
	[].
data_predicate([Name|NT], I, Arity, Constructor, DataName) -->
	{ functor(Record, Constructor, Arity),
	  arg(I, Record, Value),
	  Clause =.. [DataName, Name, Record, Value],
	  I2 is I + 1
	},
	[Clause],
	data_predicate(NT, I2, Arity, Constructor, DataName).


%%	set_predicates(+Names, +Idx0, +Arity, +Types, +Constructor)// is det.
%
%	Create the clauses
%
%		* set_<name>_of_<constructor>(Value, Old, New)
%		* set_<name>_of_<constructor>(Value, Record)

set_predicates([], _, _, _, _) -->
	[].
set_predicates([Name|NT], I, Arity, [Type|TT], Constructor) -->
	{ atomic_list_concat(['set_', Name, '_of_', Constructor], PredName),
	  atomic_list_concat(['nb_set_', Name, '_of_', Constructor], NBPredName),
	  length(Args, Arity),
	  replace_nth(I, Args, Value, NewArgs),
	  Old =.. [Constructor|Args],
	  New =.. [Constructor|NewArgs],
	  Head =.. [PredName, Value, Old, New],
	  SetHead =.. [PredName, Value, Term],
	  NBSetHead =.. [NBPredName, Value, Term],
	  (   Type == any
	  ->  Clause = Head,
	      SetClause = (SetHead :- setarg(I, Term, Value)),
	      NBSetClause = (NBSetHead :- nb_setarg(I, Term, Value))
	  ;   type_check(Type, Value, MustBe),
	      Clause = (Head :- MustBe),
	      SetClause = (SetHead :- MustBe,
				      setarg(I, Term, Value)),
	      NBSetClause = (NBSetHead :- MustBe,
				          nb_setarg(I, Term, Value))
	  ),
	  I2 is I + 1
	},
	[ Clause, SetClause, NBSetClause ],
	set_predicates(NT, I2, Arity, TT, Constructor).

type_check(Type, Value, must_be(Type, Value)) :-
	defined_type(Type, Value, _), !.
type_check(record(Spec), Value, must_be(record(M:Name), Value)) :- !,
	prolog_load_context(module, C),
	strip_module(C:Spec, M, Name).
type_check(Atom, Value, Check) :-
	atom(Atom), !,
	type_check(record(Atom), Value, Check).


%%	set_field_predicates(+Names, +Idx0, +Arity, +Types, +Constructor)// is det.
%
%	Create the clauses
%
%		* set_<constructor>_field(<name>(Value), Old, New)

set_field_predicates([], _, _, _, _) -->
	[].
set_field_predicates([Name|NT], I, Arity, [Type|TT], Constructor) -->
	{ atomic_list_concat(['set_', Constructor, '_field'], FieldPredName),
	  length(Args, Arity),
	  replace_nth(I, Args, Value, NewArgs),
	  Old =.. [Constructor|Args],
	  New =.. [Constructor|NewArgs],
	  NameTerm =.. [Name, Value],
	  SetFieldHead =.. [FieldPredName, NameTerm, Old, New],
	  (   Type == any
	  ->  SetField = SetFieldHead
	  ;   type_check(Type, Value, MustBe),
	      SetField = (SetFieldHead :- MustBe)
	  ),
	  I2 is I + 1
	},
	[ SetField ],
	set_field_predicates(NT, I2, Arity, TT, Constructor).


%%	replace_nth(+Index, +List, +Element, -NewList) is det.
%
%	Replace the Nth (1-based) element of a list.

replace_nth(1, [_|T], V, [V|T]) :- !.
replace_nth(I, [H|T0], V, [H|T]) :-
	I2 is I - 1,
	replace_nth(I2, T0, V, T).


%%	defaults(+ArgsSpecs, -Defaults, -Args)
%
%	Strip the default specification from the argument specification.

defaults([], [], []).
defaults([Arg=Default|T0], [Default|TD], [Arg|TA]) :- !,
	defaults(T0, TD, TA).
defaults([Arg|T0], [_|TD], [Arg|TA]) :-
	defaults(T0, TD, TA).


%%	types(+ArgsSpecs, -Defaults, -Args)
%
%	Strip the default specification from the argument specification.

types([], [], []).
types([Name:Type|T0], [Name|TN], [Type|TT]) :- !,
	must_be(atom, Name),
	types(T0, TN, TT).
types([Name|T0], [Name|TN], [any|TT]) :-
	must_be(atom, Name),
	types(T0, TN, TT).


		 /*******************************
		 *	      EXPANSION		*
		 *******************************/

:- multifile
	system:term_expansion/2.
:- dynamic
	system:term_expansion/2.

system:term_expansion((:- record(Record)), Clauses) :-
	compile_records(Record, Clauses).
