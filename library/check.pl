/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

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
	  list_redefined/0		% list redefinitions
	]).
:- use_module(library(lists)).
:- use_module(library(system)).
:- use_module(library(option)).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Consistency checking

This library provides some quick and dirty consistency checking of the
loaded Prolog program.

@see	prolog_xref.pl
*/

:- predicate_options(list_undefined/1, 1, [scan(oneof([local,global]))]).

%%	check
%
%	Run all consistency checks defined in this library

check :-
	print_message(informational,
		      check(pass(1, 'Undefined predicates'))),
	list_undefined(silent, global, false),
	print_message(informational,
		      check(pass(2, 'Redefined system and global predicates'))),
	list_redefined,
	print_message(informational,
		      check(pass(3, 'Predicates that need autoloading'))),
	list_autoload.

%%	list_undefined is det.
%%	list_undefined(+Options) is det.
%
%	List predicates names refered to  in  a  clause  body,  but  not
%	defined.  This forms a "Quick and Dirty" alternative for a cross
%	referencing tool.  Options:
%
%	    * scan(+Scan)
%	    If =local=, only scan the module holding the undefined
%	    predicate for references; if =global= (default), scan the
%	    whole program.  Global scanning finds references through
%	    qualified goals (i.e., M:G), but can take long on big
%	    programs.
%
%	    * unreferenced(+Boolean)
%	    If =true= (default), report undefined predicates that are
%	    not referenced by any clause body.
%
%	@see gxref/0 provides a graphical cross-referencer.
%	@see make/0 calls list_undefined/1 using scan(local)

list_undefined :-
	list_undefined(informational, global, false).

list_undefined(Options) :-
	option(scan(Scan), Options, global),
	option(unreferenced(Unreferenced), Options, false),
	list_undefined(informational, Scan, Unreferenced).


list_undefined(Level, How, Unreferenced) :-
	setup_call_cleanup(
	    ( current_prolog_flag(access_level, OldLevel),
	      set_prolog_flag(access_level, system)
	    ),
	    list_undefined_(Level, How, Unreferenced),
	    set_prolog_flag(access_level, OldLevel)).

list_undefined_(Level, How, Unreferenced) :-
	findall(Pred, undefined_predicate(Pred), Preds),
	(   Preds == []
	->  true
	;   print_message(Level, check(find_references(Preds))),
	    find_references(Preds, How, Pairs, Unrefs),
	    (	Pairs == []
	    ->	true
	    ;   print_message(warning, check(undefined_predicates)),
		(   member((Module:Head)-Refs, Pairs),
		    print_message(warning,
				  check(undefined(Module:Head, Refs))),
		    fail
		;   true
		)
	    ),
	    (	Unreferenced == true,
		Unrefs \== []
	    ->  print_message(warning, check(undefined_unreferenced_predicates)),
		(   member(Module:Head, Unrefs),
		    print_message(warning,
				  check(undefined_unreferenced(Module:Head))),
		    fail
		;   true
		)
	    ;   true
	    )
	).

undefined_predicate(Module:Head) :-
	predicate_property(Module:Head, undefined),
	\+ predicate_property(Module:Head, imported_from(_)).


%%	find_references(+Heads, +How, -HeadRefs:list, -UnrefHeads:list) is det.
%
%	Find references to the given  predicates.   For  speedup we only
%	look for references from the  same   module.  This  isn't really
%	correct, but as Module:Head  is  at   the  moment  only  handled
%	through meta-calls, it isn't too bad either.
%
%	@param HeadRefs List of Head-Refs
%	@param UnrefHeads List of Head
%	@param How is one of =local= or =global=

find_references([], _, [], []).
find_references([H|T0], How, [H-Refs|T1], T2) :-
	(   ignore(H = M:_),
	    findall(Ref, referenced(H, M, Ref), Refs)
	;   How == global,
	    findall(Ref, referenced(H, _, Ref), Refs)
	),
	Refs \== [], !,
	find_references(T0, How, T1, T2).
find_references([H|T0], How, T1, [H|T2]) :-
	find_references(T0, How, T1, T2).

%%	referenced(+Predicate, ?Module, -ClauseRef) is nondet.
%
%	True if clause ClauseRef references Predicate.

referenced(Term, Module, Ref) :-
	Goal = Module:_Head,
	current_predicate(_, Goal),
%	\+ predicate_property(Goal, built_in),
%	\+ predicate_property(Goal, imported_from(_)),
	'$get_predicate_attribute'(Goal, system, 0),
	\+ '$get_predicate_attribute'(Goal, imported, _),
	nth_clause(Goal, _, Ref),
	'$xr_member'(Ref, Term).

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


		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(check(pass(N, Comment))) -->
	[ 'PASS ~w: ~w ...~n'-[N, Comment] ].
prolog:message(check(find_references(Preds))) -->
	{ length(Preds, N)
	},
	[ 'Scanning for references to ~D possibly undefined predicates'-[N] ].
prolog:message(check(undefined_predicates)) -->
	[ 'The predicates below are not defined. If these are defined', nl,
	  'at runtime using assert/1, use :- dynamic Name/Arity.', nl, nl
	].
prolog:message(check(undefined(Pred, Refs))) -->
	predicate(Pred),
	[ ', which is referenced by', nl ],
	referenced_by(Refs).
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

redefined(user, system) -->
	[ '~t~30| System predicate redefined globally' ].
redefined(_, system) -->
	[ '~t~30| Redefined system predicate' ].
redefined(_, user) -->
	[ '~t~30| Redefined global predicate' ].

predicate(user:Name/Arity) -->
	{ atom(Name),
	  integer(Arity)
	}, !,
	[ '~q/~d'-[Name, Arity] ].
predicate(Module:Name/Arity) -->
	{ atom(Module),
	  atom(Name),
	  integer(Arity)
	}, !,
	[ '~q:~q/~d'-[Module, Name, Arity] ].
predicate(Module:Head) -->
	{ atom(Module),
	  callable(Head),
	  functor(Head, Name, Arity)
	}, !,
	predicate(Module:Name/Arity).
predicate(Name/Arity) -->
	{ atom(Name),
	  integer(Arity)
	}, !,
	[ '~q/~d'-[Name, Arity] ].

autoload([]) -->
	[].
autoload([Lib-Pred|T]) -->
	[ '    ' ],
	predicate(Pred),
	[ '~t~24| from ' ],
	short_filename(Lib),
	[ nl ],
	autoload(T).

referenced_by([]) -->
	[].
referenced_by([Ref|T]) -->
	{ nth_clause(M:Head, N, Ref)
	},
	(   { clause_property(Ref, file(Path)),
	      clause_property(Ref, line_count(Line))
	    }
	->  ['\t'-[] ], predicate(M:Head),
	    [' at ~w:~w'-[Path, Line] ]
	;   { suffix(N, Suff)
	    },
	    [ '\t~d-~w clause of '-[N, Suff] ],
	    predicate(M:Head)
	),
	[ nl ],
	referenced_by(T).

suffix(1, st) :- !.
suffix(2, nd) :- !.
suffix(_, th).

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
