/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University, Amsterdam

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

:- module(persistency,
	  [ (persistent)/1,		% +Declarations
	    current_persistent_predicate/1, % :PI

	    db_attach/2,		% :File, +Options

	    db_sync/1,			% :What
	    db_sync_all/1,		% +What

	    op(1150, fx, (persistent))
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(aggregate)).

:- predicate_options(db_attach/2, 2,
		     [ sync(oneof([close,flush,none]))
		     ]).

/** <module> Provide persistent dynamic predicates

This module provides simple persistent storage   for one or more dynamic
predicates. A database is always associated with a module. A module that
wishes to maintain a database must declare  the terms that can be placed
in the database using the directive persistent/1.

The persistent/1 expands each declaration into four predicates:

	* name(Arg, ...)
	* assert_name(Arg, ...)
	* retract_name(Arg, ...)
	* retractall_name(Arg, ...)

As mentioned, a database can  only  be   accessed  from  within a single
module. This limitation is on purpose,  forcing   the  user to provide a
proper API for accessing the shared persistent data.

Below is a simple example:

==
:- module(user_db,
	  [ attach_user_db/1,		% +File
	    current_user_role/2,	% ?User, ?Role
	    add_user/2,			% +User, +Role
	    set_user_role/2		% +User, +Role
	  ]).

:- persistent
	user_role(name:atom, role:oneof([user,administrator])).

attach_user_db(File) :-
	db_attach(File, []).

%%	current_user_role(+Name, -Role) is semidet.

current_user_role(Name, Role) :-
	with_mutex(user_db, user_role(Name, Role)).

add_user(Name, Role) :-
	assert_user_role(Name, Role).

set_user_role(Name, Role) :-
	user_role(Name, Role), !.
set_user_role(Name, Role) :-
	with_mutex(user_db,
		   (  retractall_user_role(Name, _),
		      assert_user_role(Name, Role))).
==

@tbd	Provide type safety while loading
@tbd	Thread safety must now be provided at the user-level. Can we
	provide generic thread safety?  Basically, this means that we
	must wrap all exported predicates.  That might better be done
	outside this library.
@tbd	Transaction management?
@tbd	Should assert_<name> only assert if the database does not
	contain a variant?
*/

:- meta_predicate
	db_attach(:, +),
	db_sync(:),
	current_persistent_predicate(:).


		 /*******************************
		 *		DB		*
		 *******************************/

:- dynamic
	db_file/3,			% Module, File, Modified
	db_stream/2,			% Module, Stream
	db_dirty/2,			% Module, Deleted
	db_option/2.			% Module, Name(Value)

:- volatile
	db_stream/2.

:- multifile
	(persistent)/3,			% Module, Generic, Term
	prolog:generated_predicate/1.


		 /*******************************
		 *	   DECLARATIONS		*
		 *******************************/

%%	persistent(+Spec)
%
%	Declare dynamic database terms. Declarations appear in a
%	directive and have the following format:
%
%	==
%	:- persistent
%		<callable>,
%		<callable>,
%		...
%	==
%
%	Each specification is a callable term, following the conventions
%	of library(record), where each argument is of the form
%
%		name:type
%
%	Types are defined by library(error).

persistent(Spec) :-
	throw(error(context_error(nodirective, persistent(Spec)), _)).

compile_persistent(Var, _) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
compile_persistent((A,B), Module) --> !,
	compile_persistent(A, Module),
	compile_persistent(B, Module).
compile_persistent(Term, Module) -->
	{ functor(Term, Name, Arity),		% Validates Term as callable
	  functor(Generic, Name, Arity)
	},
	[ :- dynamic(Name/Arity),

	  persistency:persistent(Module, Generic, Term)
	],
	assert_clause(Term, Module),
	retract_clause(Term, Module),
	retractall_clause(Term, Module).

assert_clause(Term, Module) -->
	{ functor(Term, Name, Arity),
	  atom_concat(assert_, Name, PredName),
	  length(Args, Arity),
	  Head =.. [PredName|Args],
	  Assert =.. [Name|Args],
	  type_checkers(Args, 1, Term, Check),
	  Clause = (Head :- Check, persistency:db_assert(Module:Assert))
	},
	[ Clause ].

type_checkers([], _, _, true).
type_checkers([A0|AL], I, Spec, Check) :-
	arg(I, Spec, ArgSpec),
	(   ArgSpec = _Name:Type,
	    nonvar(Type),
	    Type \== any
	->  Check = (must_be(Type, A0),More)
	;   More = Check
	),
	I2 is I + 1,
	type_checkers(AL, I2, Spec, More).

retract_clause(Term, Module) -->
	{ functor(Term, Name, Arity),
	  atom_concat(retract_, Name, PredName),
	  length(Args, Arity),
	  Head =.. [PredName|Args],
	  Retract =.. [Name|Args],
	  Clause = (Head :- persistency:db_retract(Module:Retract))
	},
	[ Clause ].

retractall_clause(Term, Module) -->
	{ functor(Term, Name, Arity),
	  atom_concat(retractall_, Name, PredName),
	  length(Args, Arity),
	  Head =.. [PredName|Args],
	  Retract =.. [Name|Args],
	  Clause = (Head :- persistency:db_retractall(Module:Retract))
	},
	[ Clause ].

:- multifile
	user:term_expansion/2.

user:term_expansion((:- persistent(Spec)), Clauses) :-
	prolog_load_context(module, Module),
	phrase(compile_persistent(Spec, Module), Clauses).


%%	current_persistent_predicate(:PI) is nondet.
%
%	True if PI is a predicate that provides access to the persistent
%	database DB.

current_persistent_predicate(M:PName/Arity) :-
	persistency:persistent(M, Generic, _),
	functor(Generic, Name, Arity),
	(   Name = PName
	;   atom_concat(assert_, Name, PName)
	;   atom_concat(retract_, Name, PName)
	;   atom_concat(retractall_, Name, PName)
	).

prolog:generated_predicate(PI) :-
	current_persistent_predicate(PI).


		 /*******************************
		 *	      ATTACH		*
		 *******************************/

%%	db_attach(:File, +Options)
%
%	Use File as persistent database  for   the  calling  module. The
%	calling module must defined persistent/1   to  declare the database
%	terms.  Defined options:
%
%		* sync(+Sync)
%		One of =close= (close journal after write), =flush=
%		(default, flush journal after write) or =none=
%		(handle as fully buffered stream).

db_attach(Module:File, Options) :-
	db_set_options(Module, Options),
	db_attach_file(Module, File).

db_set_options(Module, Options) :-
	retractall(db_option(Module, _)),
	option(sync(Sync), Options, flush),
	must_be(oneof([close,flush,none]), Sync),
	assert(db_option(Module, sync(Sync))).

db_attach_file(Module, File) :-
	db_file(Module, Old, _), !,		% we already have a db
	(   Old == File
	->  true
	;   permission_error(attach, db, File)
	).
db_attach_file(Module, File) :-
	db_load(Module, File), !.
db_attach_file(Module, File) :-
	assert(db_file(Module, File, 0)).

db_load(Module, File) :-
	retractall(db_file(Module, _, _)),
	catch(open(File, read, In, [encoding(utf8)]), _, fail), !,
	debug(db, 'Loading database ~w', [File]),
	call_cleanup((read_action(In, T0),
		      load_db(T0, In, Module)),
		     close(In)),
	debug(db, 'Loaded ~w', [File]),
	time_file(File, Modified),
	assert(db_file(Module, File, Modified)).

load_db(end_of_file, _, _) :- !.
load_db(assert(Term), In, Module) :-
	persistent(Module, Term, _Types), !,
	assert(Module:Term),
	read_action(In, T1),
	load_db(T1, In, Module).
load_db(retractall(Term, Count), In, Module) :-
	persistent(Module, Term, _Types), !,
	retractall(Module:Term),
	set_dirty(Module, Count),
	read_action(In, T1),
	load_db(T1, In, Module).
load_db(retract(Term), In, Module) :-
	persistent(Module, Term, _Types), !,
	(   retract(Module:Term)
	->  set_dirty(Module, 1)
	;   true
	),
	read_action(In, T1),
	load_db(T1, In, Module).
load_db(Term, In, Module) :-
	print_message(error, illegal_term(Term)),
	read_action(In, T1),
	load_db(T1, In, Module).

db_clean(Module) :-
	retractall(db_dirty(Module, _)),
	(   persistent(Module, Term, _Types),
	    retractall(Module:Term),
	    fail
	;   true
	).

%%	db_size(+Module, -Terms) is det.
%
%	Terms is the total number of terms in the DB for Module.

db_size(Module, Total) :-
	aggregate_all(sum(Count), persistent_size(Module, Count), Total).

persistent_size(Module, Count) :-
	persistent(Module, Term, _Types),
	predicate_property(Module:Term, number_of_clauses(Count)).

%%	db_assert(:Term) is det.
%
%	Assert Term into the database  and   record  it for persistency.
%	Note that if the on-disk file  has   been  modified  it is first
%	reloaded.

db_assert(Module:Term) :-
	assert(Module:Term),
	persistent(Module, assert(Term)).

persistent(Module, Action) :-
	(   db_stream(Module, Stream)
	->  true
	;   db_file(Module, File, _Modified)
	->  db_sync(Module, reload),		% Is this correct?
	    open(File, append, Stream,
		 [ close_on_abort(false),
		   encoding(utf8),
		   lock(write)
		 ]),
	    assert(db_stream(Module, Stream))
	;   existence_error(db_file, Module)
	),
	write_action(Stream, Action),
	sync(Module, Stream).

%%	sync(+Module, +Stream) is det.
%
%	Synchronise journal after a write.   Using  =close=, the journal
%	file is closed, making it easier   to  edit the file externally.
%	Using =flush= flushes the stream  but   does  not close it. This
%	provides better performance. Using  =none=,   the  stream is not
%	even flushed. This makes the journal   sensitive to crashes, but
%	much faster.

sync(Module, Stream) :-
	db_option(Module, sync(Sync)),
	(   Sync == close
	->  db_sync(Module, close)
	;   Sync == flush
	->  flush_output(Stream)
	;   true
	).

read_action(Stream, Action) :-
	read_term(Stream, Action, [module(db)]).

write_action(Stream, Action) :-
	\+ \+ ( numbervars(Action, 0, _, [singletons(true)]),
		format(Stream, '~W.~n',
		       [ Action,
			 [ quoted(true),
			   numbervars(true),
			   module(db)
			 ]
		       ])
	      ).

%%	db_retractall(:Term) is det.
%
%	Retract all matching facts and do the   same in the database. If
%	Term is unbound, persistent/1 from the   calling  module is used as
%	generator.

db_retractall(Module:Term) :-
	(   var(Term)
	->  forall(persistent(Module, Term, _Types),
		   db_retractall(Module:Term))
	;   State = count(0),
	    (	retract(Module:Term),
		arg(1, State, C0),
		C1 is C0+1,
		nb_setarg(1, State, C1),
		fail
	    ;	arg(1, State, Count)
	    ),
	    (	Count > 0
	    ->	set_dirty(Module, Count),
		persistent(Module, retractall(Term, Count))
	    ;	true
	    )
	).


%%	db_retract(:Term) is nondet.
%
%	Retract terms from the database one-by-one.

db_retract(Module:Term) :-
	(   var(Term)
	->  instantiation_error(Term)
	;   retract(Module:Term),
	    set_dirty(Module, 1),
	    persistent(Module, retract(Term))
	).


set_dirty(_, 0) :- !.
set_dirty(Module, Count) :-
	(   retract(db_dirty(Module, C0))
	->  true
	;   C0 = 0
	),
	C1 is C0 + Count,
	assert(db_dirty(Module, C1)).

%%	db_sync(:What)
%
%	Synchronise database with the associated file.  What is one of:
%
%		* reload
%		Database is reloaded from file
%		* gc
%		Database was re-written, deleting all retractall
%		statements.  This is the same as gc(50).
%		* gc(Percentage)
%		GC DB if the number of deleted terms is the given
%		percentage of the total number of terms.
%		* close
%		Database stream was closed
%		* nop
%		No-operation performed
%
%	With unbound What, db_sync/1 reloads  the   database  if  it was
%	modified on disk, gc it if it  is   dirty  and close it if it is
%	opened.

db_sync(Module:What) :-
	db_sync(Module, What).


db_sync(Module, reload) :-
	\+ db_stream(Module, _),		% not open
	db_file(Module, File, ModifiedWhenLoaded),
	catch(time_file(File, Modified), _, fail),
	Modified > ModifiedWhenLoaded, !,	% Externally modified
	debug(db, 'Database ~w was externally modified; reloading', [File]),
	db_clean(Module),
	db_load(Module, File).
db_sync(Module, gc) :-
	db_sync(Module, gc(50)).
db_sync(Module, gc(When)) :-
	db_dirty(Module, Dirty),
	(   When == always
	->  true
	;   db_size(Module, Total),
	    Perc is (100*Dirty)/Total,
	    Perc > When
	),
	db_sync(Module, close),
	db_file(Module, File, Modified),
	atom_concat(File, '.new', NewFile),
	debug(db, 'Database ~w is dirty; cleaning', [File]),
	open(NewFile, write, Out, [encoding(utf8)]),
	(   persistent(Module, Term, _Types),
	    Module:Term,
	    write_action(Out, assert(Term)),
	    fail
	;   true
	),
	close(Out),
	retractall(db_file(Module, File, Modified)),
	rename_file(NewFile, File),
	time_file(File, NewModified),
	assert(db_file(Module, File, NewModified)).
db_sync(Module, close) :-
	retract(db_stream(Module, Stream)), !,
	db_file(Module, File, _),
	debug(db, 'Database ~w is open; closing', [File]),
	close(Stream),
	time_file(File, Modified),
	retractall(db_file(Module, File, _)),
	assert(db_file(Module, File, Modified)).
db_sync(_, nop) :- !.
db_sync(_, _).


%%	db_sync_all(+What)
%
%	Sync all registered databases.

db_sync_all(What) :-
	must_be(oneof([reload,gc,gc(_),close]), What),
	forall(db_file(Module, _, _),
	       db_sync(Module:What)).


		 /*******************************
		 *	       CLOSE		*
		 *******************************/

close_dbs :-
	forall(retract(db_stream(_Module, Stream)),
	       close(Stream)).

:- at_halt(close_dbs).
