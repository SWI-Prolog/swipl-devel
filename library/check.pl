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

:- module(check,
	[ check/0,			% run all checks
	  list_undefined/0,		% list undefined predicates
	  list_autoload/0,		% list predicates that need autoloading
	  list_redefined/0		% list redefinitions
	]).
:- use_module(library(lists)).

:- style_check(+dollar).		% lock these predicates

%	check/0
%	
%	Run all consistency checks defined in this library

check :-
	print_message(informational,
		      check(pass(1, 'Undefined predicates'))),
	list_undefined,
	print_message(informational,
		      check(pass(2, 'Redefined system and global predicates'))),
	list_redefined,
	print_message(informational,
		      check(pass(3, 'Predicates that need autoloading'))),
	list_autoload.

%	list_undefined/0
%	
%	List predicates names refered to  in  a  clause  body,  but  not
%	defined.  This forms a "Quick and Dirty" alternative for a cross
%	referencing tool (which I need to write someday).

list_undefined :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	call_cleanup(list_undefined_, $style_check(_, Old)).

list_undefined_ :-
	findall(Pred, undefined_predicate(Pred), Preds),
	(   Preds == []
	->  true
	;   print_message(informational, check(find_references(Preds))),
	    find_references(Preds, Pairs),
	    (	Pairs == []
	    ->	true
	    ;   print_message(warning, check(undefined_predicates)),
		(   member((Module:Head)-Refs, Pairs),
		    print_message(warning,
				  check(undefined(Module:Head, Refs))),
		    fail
		;   true
		)
	    )
	).

undefined_predicate(Module:Head) :-
	predicate_property(Module:Head, undefined), 
	\+ predicate_property(Module:Head, imported_from(_)),
	functor(Head, Functor, Arity), 
	\+ $in_library(Functor, Arity),
	\+ system_undefined(Module:Functor/Arity).

system_undefined(user:prolog_trace_interception/4).

%	find_references(+Heads, -Head-Refs)
%	
%	Find references to the given  predicates.   For  speedup we only
%	look for references from the  same   module.  This  isn't really
%	correct, but as Module:Head  is  at   the  moment  only  handled
%	through meta-calls, it isn't too bad either.

find_references([], []).
find_references([H|T0], [H-Refs|T]) :-
	ignore(H = M:_),
	findall(Ref, referenced(H, M, Ref), Refs),
	Refs \== [], !,
	find_references(T0, T).
find_references([_|T0], T) :-
	find_references(T0, T).

%	referenced(+Predicate, ?Module, -ClauseRef)
%
%	True if Clause ClauseRef references Predicate.

referenced(Term, Module, Ref) :-
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, built_in),
	\+ predicate_property(Module:Head, imported_from(_)),
	nth_clause(Module:Head, _, Ref),
	'$xr_member'(Ref, Term).

%	list_autoload/0
%	
%	Show predicates that need be linked via the autoload mechanism

list_autoload :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	current_prolog_flag(autoload, OldAutoLoad),
	set_prolog_flag(autoload, false),
	call_cleanup(list_autoload_, 
		     (	 set_prolog_flag(autoload, OldAutoLoad),
			 $style_check(_, Old)
		     )).
	
list_autoload_ :-
	(   setof(Lib-Pred, autoload_predicate(Module, Lib, Pred), Pairs),
	    print_message(informational,
			  check(autoload(Module, Pairs))),
	    fail
	;   true
	).

autoload_predicate(Module, Library, Name/Arity) :-
	predicate_property(Module:Head, undefined), 
	(   \+ predicate_property(Module:Head, imported_from(_)), 
	    functor(Head, Name, Arity), 
	    $in_library(Name, Arity),
	    $find_library(Module, Name, Arity, _LoadModule, Library),
	    referenced(Module:Head, Module, _)
	->  true
	).


%	list_redefined/0
%	
%	Show redefined system predicates

list_redefined :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	call_cleanup(list_redefined_, $style_check(_, Old)).
	
list_redefined_ :-
	current_module(Module),
	Module \== system,
	current_predicate(_, Module:Head),
	\+ predicate_property(Module:Head, imported_from(_)),
	(   $default_module(Module, Super, Super),
	    $c_current_predicate(_, Super:Head),
	    $syspreds:$defined_predicate(Super:Head),
	    \+ predicate_property(Super:Head, (dynamic)),
	    \+ predicate_property(Super:Head, imported_from(Module)),
	    functor(Head, Name, Arity)
	->  print_message(informational,
			  check(redefined(Module, Super, Name/Arity)))
	),
	fail.
list_redefined_.


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
	[ 'Scanning references for ~D possibly undefined predicates'-[N] ].
prolog:message(check(undefined_predicates)) -->
	[ 'The predicates below are not defined. If these are defined', nl,
	  'at runtime using assert/1, use :- dynamic Name/Arity.', nl, nl
	].
prolog:message(check(undefined(Pred, Refs))) -->
	predicate(Pred),
	[ ', which is referenced by', nl ],
	referenced_by(Refs).
prolog:message(check(autoload(Module, Pairs))) -->
	{ current_module(Module, Path)
	}, !,
	[ 'Into module ~w ('-[Module] ],
	short_filename(Path),
	[ ')', nl ],
	autoload(Pairs).
prolog:message(check(autoload(Module, Pairs))) -->
	[ 'Into module ~w'-[Module], nl ],
	autoload(Pairs).
prolog:message(check(redefined(In, From, Pred))) -->
	predicate(Pred),
	redefined(In, From).

redefined(user, system) -->
	[ '~t~30| System predicate redefined globally' ].
redefined(M, system) -->
	[ '~t~30| System predicate redefined in ~w'-[M] ].
redefined(M, user) -->
	[ '~t~30| Global predicate redefined in ~w'-[M] ].

predicate(user:Name/Arity) --> !,
	[ '~q/~d'-[Name, Arity] ].
predicate(Module:Name/Arity) --> !,
	[ '~q:~q/~d'-[Module, Name, Arity] ].
predicate(Module:Head) --> !,
	{ functor(Head, Name, Arity)
	},
	predicate(Module:Name/Arity).
predicate(Name/Arity) --> !,
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
	{ nth_clause(M:Head, N, Ref),
	  suffix(N, Suff)
	},
	[ '        ~d-~w clause of '-[N, Suff] ],
	predicate(M:Head),
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
	setof(Alias, Spec^file_search_path(Alias, Spec), Aliases),
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
