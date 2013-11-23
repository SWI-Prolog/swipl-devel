/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2012, University of Amsterdam
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

:- module(check,
	[ check/0,			% run all checks
	  list_undefined/0,		% list undefined predicates
	  list_undefined/1,		% +Options
	  list_autoload/0,		% list predicates that need autoloading
	  list_redefined/0,		% list redefinitions
	  list_trivial_fails/0,		% list goals that trivially fail
	  list_trivial_fails/1		% +Options
	]).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(option)).
:- use_module(library(apply)).
:- use_module(library(prolog_codewalk)).

:- set_prolog_flag(generate_debug_info, false).

:- multifile trivial_fail_goal/1.

/** <module> Consistency checking

This library provides some quick and dirty consistency checking of the
loaded Prolog program.

@see	prolog_xref.pl
*/

:- predicate_options(list_undefined/1, 1, [scan(oneof([local,global]))]).

%%	check is det.
%
%	Run all consistency checks defined in this library.  Currently
%	defined checks are:
%
%	  * list_undefined/0 reports undefined predicates
%	  * list_trivial_fails/0 reports calls for which there is no
%	    matching clause.
%	  * list_redefined/0 reports predicates that have a local
%	    definition and a global definition.  Note that these are
%	    *not* errors.
%	  * list_autoload/0 lists predicates that will be defined at
%	    runtime using the autoloader.

check :-
	print_message(informational,
		      check(pass(1, 'Undefined predicates'))),
	list_undefined,
	print_message(informational,
		      check(pass(2, 'Trivial failures'))),
	list_trivial_fails,
	print_message(informational,
		      check(pass(3, 'Redefined system and global predicates'))),
	list_redefined,
	print_message(informational,
		      check(pass(4, 'Predicates that need autoloading'))),
	list_autoload.

%%	list_undefined is det.
%%	list_undefined(+Options) is det.
%
%	List predicates names refered to  in  a  clause  body,  but  not
%	defined.  This forms a "Quick and Dirty" alternative for a cross
%	referencing tool.  Options:
%
%	    * module_class(+Classes)
%	    Process modules of the given Classes.  The default for
%	    classes is =|[user]|=. For example, to include the
%	    libraries into the examination, use =|[user,library]|=.
%
%	@see gxref/0 provides a graphical cross-referencer.
%	@see make/0 calls list_undefined/0

:- thread_local
	undef/2.

list_undefined :-
	list_undefined([]).

list_undefined(Options) :-
	merge_options(Options,
		      [ module_class([user])
		      ],
		      WalkOptions),
	prolog_walk_code([ undefined(trace),
			   on_trace(found_undef)
			 | WalkOptions
			 ]),
	findall(PI-From, retract(undef(PI, From)), Pairs),
	(   Pairs == []
	->  true
	;   print_message(warning, check(undefined_predicates)),
	    keysort(Pairs, Sorted),
	    group_pairs_by_key(Sorted, Grouped),
	    maplist(report_undefined, Grouped)
	).

:- public found_undef/3.

found_undef(To, _Caller, From) :-
	goal_pi(To, PI),
	(   undef(PI, From)
	->  true
	;   assertz(undef(PI,From))
	).

goal_pi(M:Head, M:Name/Arity) :-
	functor(Head, Name, Arity).

report_undefined(PI-FromList) :-
	print_message(warning, check(undefined(PI, FromList))).


%%	list_autoload
%
%	Show predicates that are not defined, but will be loaded on
%	demand through the autoloader.
%
%	The behaviour of this predicate depends  on the system-mode (see
%	system_mode/1): in normal  operation  it   only  lists  autoload
%	requirements from user-module. In system-mode it also lists such
%	requirements for the system modules.

list_autoload :-
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, OldLevel),
	      current_prolog_flag(autoload, OldAutoLoad),
	      set_prolog_flag(access_level, system),
	      set_prolog_flag(autoload, false)
	    ),
	    list_autoload_(OldLevel),
	    ( set_prolog_flag(access_level, OldLevel),
	      set_prolog_flag(autoload, OldAutoLoad)
	    )).

list_autoload_(SystemMode) :-
	(   setof(Lib-Pred,
		  autoload_predicate(Module, Lib, Pred, SystemMode),
		  Pairs),
	    print_message(informational,
			  check(autoload(Module, Pairs))),
	    fail
	;   true
	).

autoload_predicate(Module, Library, Name/Arity, SystemMode) :-
	predicate_property(Module:Head, undefined),
	check_module_enabled(Module, SystemMode),
	(   \+ predicate_property(Module:Head, imported_from(_)),
	    functor(Head, Name, Arity),
	    '$find_library'(Module, Name, Arity, _LoadModule, Library),
	    referenced(Module:Head, Module, _)
	->  true
	).

check_module_enabled(_, system) :- !.
check_module_enabled(Module, _) :-
	\+ import_module(Module, system).

%%      referenced(+Predicate, ?Module, -ClauseRef) is nondet.
%
%       True if clause ClauseRef references Predicate.

referenced(Term, Module, Ref) :-
        Goal = Module:_Head,
        current_predicate(_, Goal),
        '$get_predicate_attribute'(Goal, system, 0),
        \+ '$get_predicate_attribute'(Goal, imported, _),
        nth_clause(Goal, _, Ref),
        '$xr_member'(Ref, Term).

%%	list_redefined
%
%	Show redefined system predicates

list_redefined :-
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, OldLevel),
	      set_prolog_flag(access_level, system)
	    ),
	    list_redefined_,
	    set_prolog_flag(access_level, OldLevel)).

list_redefined_ :-
	current_module(Module),
	Module \== system,
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, imported_from(_)),
	(   global_module(Super),
	    Super \== Module,
	    '$c_current_predicate'(_, Super:Head),
	    \+ redefined_ok(Head),
	    '$syspreds':'$defined_predicate'(Super:Head),
	    \+ predicate_property(Super:Head, (dynamic)),
	    \+ predicate_property(Super:Head, imported_from(Module)),
	    functor(Head, Name, Arity)
	->  print_message(informational,
			  check(redefined(Module, Super, Name/Arity)))
	),
	fail.
list_redefined_.

redefined_ok('$mode'(_,_)).
redefined_ok('$pldoc'(_,_,_,_)).
redefined_ok('$pred_option'(_,_,_,_)).

global_module(user).
global_module(system).

%%	list_trivial_fails is det.
%%	list_trivial_fails(+Options) is det.
%
%	List goals that trivially fail  because   there  is  no matching
%	clause.  Options:
%
%	  * module_class(+Classes)
%	    Process modules of the given Classes.  The default for
%	    classes is =|[user]|=. For example, to include the
%	    libraries into the examination, use =|[user,library]|=.

:- thread_local
	trivial_fail/2.

list_trivial_fails :-
	list_trivial_fails([]).

list_trivial_fails(Options) :-
	merge_options(Options,
		      [ module_class([user]),
			infer_meta_predicates(false),
			autoload(false),
			evaluate(false),
			trace_reference(_),
			on_trace(check_trivial_fail)
		      ],
		      WalkOptions),

	prolog_walk_code([ source(false)
			 | WalkOptions
			 ]),
	findall(CRef, retract(trivial_fail(clause(CRef), _)), Clauses),
	(   Clauses == []
	->  true
	;   print_message(warning, check(trivial_failures)),
	    prolog_walk_code([ clauses(Clauses)
			     | WalkOptions
			     ]),
	    findall(Goal-From, retract(trivial_fail(From, Goal)), Pairs),
	    keysort(Pairs, Sorted),
	    group_pairs_by_key(Sorted, Grouped),
	    maplist(report_trivial_fail, Grouped)
	).

%%	trivial_fail_goal(:Goal)
%
%	Multifile hook that tells list_trivial_fails/0 to accept Goal as
%	valid.

trivial_fail_goal(pce_expansion:pce_class(_, _, template, _, _, _)).
trivial_fail_goal(pce_host:property(system_source_prefix(_))).

:- public
	check_trivial_fail/3.

check_trivial_fail(MGoal0, _Caller, From) :-
	(   MGoal0 = M:Goal,
	    atom(M),
	    callable(Goal),
            predicate_property(MGoal0, interpreted),
	    \+ predicate_property(MGoal0, dynamic),
	    \+ predicate_property(MGoal0, multifile),
	    \+ trivial_fail_goal(MGoal0)
	->  (	predicate_property(MGoal0, meta_predicate(Meta))
	    ->  qualify_meta_goal(MGoal0, Meta, MGoal)
	    ;   MGoal = MGoal0
	    ),
	    (	clause(MGoal, _)
	    ->  true
	    ;   assertz(trivial_fail(From, MGoal))
	    )
	;   true
	).

report_trivial_fail(Goal-FromList) :-
	print_message(warning, check(trivial_failure(Goal, FromList))).

%%	qualify_meta_goal(+Module, +MetaSpec, +Goal, -QualifiedGoal)
%
%	Qualify a goal if the goal calls a meta predicate

qualify_meta_goal(M:Goal0, Meta, M:Goal) :-
	functor(Goal0, F, N),
	functor(Goal, F, N),
	qualify_meta_goal(1, M, Meta, Goal0, Goal).

qualify_meta_goal(N, M, Meta, Goal0, Goal) :-
	arg(N, Meta,  ArgM), !,
	arg(N, Goal0, Arg0),
	arg(N, Goal,  Arg),
	N1 is N + 1,
	(   module_qualified(ArgM)
	->  add_module(Arg0, M, Arg)
	;   Arg = Arg0
	),
	meta_goal(N1, Meta, Goal0, Goal).
meta_goal(_, _, _, _).

add_module(Arg, M, M:Arg) :-
	var(Arg), !.
add_module(M:Arg, _, MArg) :- !,
	add_module(Arg, M, MArg).
add_module(Arg, M, M:Arg).

module_qualified(N) :- integer(N), !.
module_qualified(:).
module_qualified(^).


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(check(pass(N, Comment))) -->
	[ 'PASS ~w: ~w ...'-[N, Comment] ].
prolog:message(check(find_references(Preds))) -->
	{ length(Preds, N)
	},
	[ 'Scanning for references to ~D possibly undefined predicates'-[N] ].
prolog:message(check(undefined_predicates)) -->
	[ 'The predicates below are not defined. If these are defined', nl,
	  'at runtime using assert/1, use :- dynamic Name/Arity.', nl, nl
	].
prolog:message(check(undefined(Pred, Refs))) -->
	{ map_list_to_pairs(sort_reference_key, Refs, Keyed),
	  keysort(Keyed, KeySorted),
	  pairs_values(KeySorted, SortedRefs)
	},
	predicate(Pred),
	[ ', which is referenced by', nl ],
	referenced_by(SortedRefs).
prolog:message(check(undefined_unreferenced_predicates)) -->
	[ 'The predicates below are not defined, and are not', nl,
	  'referenced.', nl, nl
	].
prolog:message(check(undefined_unreferenced(Pred))) -->
	predicate(Pred).
prolog:message(check(autoload(Module, Pairs))) -->
	{ module_property(Module, file(Path))
	}, !,
	[ 'Into module ~w ('-[Module] ],
	short_filename(Path),
	[ ')', nl ],
	autoload(Pairs).
prolog:message(check(autoload(Module, Pairs))) -->
	[ 'Into module ~w'-[Module], nl ],
	autoload(Pairs).
prolog:message(check(redefined(In, From, Pred))) -->
	predicate(In:Pred),
	redefined(In, From).
prolog:message(check(trivial_failures)) -->
	[ 'The following goals fail because there are no matching clauses.' ].
prolog:message(check(trivial_failure(Goal, Refs))) -->
	{ map_list_to_pairs(sort_reference_key, Refs, Keyed),
	  keysort(Keyed, KeySorted),
	  pairs_values(KeySorted, SortedRefs)
	},
	goal(Goal),
	[ ', which is called from'-[], nl ],
	referenced_by(SortedRefs).

redefined(user, system) -->
	[ '~t~30| System predicate redefined globally' ].
redefined(_, system) -->
	[ '~t~30| Redefined system predicate' ].
redefined(_, user) -->
	[ '~t~30| Redefined global predicate' ].

goal(user:Goal) --> !,
	[ '~p'-[Goal] ].
goal(Goal) --> !,
	[ '~p'-[Goal] ].

predicate(Module:Name/Arity) -->
	{ atom(Module),
	  atom(Name),
	  integer(Arity),
	  functor(Head, Name, Arity),
	  predicate_name(Module:Head, PName)
	}, !,
	[ '~w'-[PName] ].
predicate(Module:Head) -->
	{ atom(Module),
	  callable(Head),
	  predicate_name(Module:Head, PName)
	}, !,
	[ '~w'-[PName] ].
predicate(Name/Arity) -->
	{ atom(Name),
	  integer(Arity)
	}, !,
	predicate(user:Name/Arity).

autoload([]) -->
	[].
autoload([Lib-Pred|T]) -->
	[ '    ' ],
	predicate(Pred),
	[ '~t~24| from ' ],
	short_filename(Lib),
	[ nl ],
	autoload(T).

%%	sort_reference_key(+Reference, -Key) is det.
%
%	Create a stable key for sorting references to predicates.

sort_reference_key(Term, key(M:Name/Arity, N, ClausePos)) :-
	clause_ref(Term, ClauseRef, ClausePos), !,
	nth_clause(Pred, N, ClauseRef),
	strip_module(Pred, M, Head),
	functor(Head, Name, Arity).
sort_reference_key(Term, Term).

clause_ref(clause_term_position(ClauseRef, TermPos), ClauseRef, ClausePos) :-
	arg(1, TermPos, ClausePos).
clause_ref(clause(ClauseRef), ClauseRef, 0).


referenced_by([]) -->
	[].
referenced_by([Ref|T]) -->
	['\t'], prolog:message_location(Ref),
	        predicate_indicator(Ref),
	[ nl ],
	referenced_by(T).

predicate_indicator(clause_term_position(ClauseRef, _)) -->
	{ nonvar(ClauseRef) }, !,
	predicate_indicator(clause(ClauseRef)).
predicate_indicator(clause(ClauseRef)) -->
	{ clause_name(ClauseRef, Name) },
	[ '~w'-[Name] ].
predicate_indicator(file_term_position(_,_)) -->
	[ '(initialization)' ].
predicate_indicator(file(_,_,_,_)) -->
	[ '(initialization)' ].


short_filename(Path) -->
	{ short_filename(Path, Spec)
	},
	[ '~q'-[Spec] ].

short_filename(Path, Spec) :-
	absolute_file_name('', Here),
	atom_concat(Here, Local0, Path), !,
	remove_leading_slash(Local0, Spec).
short_filename(Path, Spec) :-
	findall(LenAlias, aliased_path(Path, LenAlias), Keyed),
	keysort(Keyed, [_-Spec|_]).
short_filename(Path, Path).

aliased_path(Path, Len-Spec) :-
	setof(Alias, Spec^(user:file_search_path(Alias, Spec)), Aliases),
	member(Alias, Aliases),
	Term =.. [Alias, '.'],
	absolute_file_name(Term,
			   [ file_type(directory),
			     file_errors(fail),
			     solutions(all)
			   ], Prefix),
	atom_concat(Prefix, Local0, Path),
	remove_leading_slash(Local0, Local),
	atom_length(Local, Len),
	Spec =.. [Alias, Local].

remove_leading_slash(Path, Local) :-
	atom_concat(/, Local, Path), !.
remove_leading_slash(Path, Path).
