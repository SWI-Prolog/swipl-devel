/*  $Id$

    Copyright (c) 1990 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: Simple program consistency check predicates
*/

:- module(check,
	[ check/0			% run all checks
        , list_undefined/0		% list undefined predicates
	, list_autoload/0		% list predicates that need autoloading
	, list_redefined/0		% list redefinitions
	]).

:- style_check(+dollar).		% lock these predicates

%	check
%	run all consistency checks of this module

check :-
	format('PASS 1: Scanning for undefined predicates ...~n'),
	list_undefined,
	format('~nPASS 2: Scanning for redefined system and global predicates ...~n'),
	list_redefined,
	format('~nPASS 3: Scanning for predicates that need autoloading ...~n'),
	list_autoload.

%	list_undefined
%	List predicates names refered to  in  a  clause  body,  but  not
%	defined.  This forms a "Quick and Dirty" alternative for a cross
%	referencing tool (which I need to write someday).

list_undefined :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	list_undefined_, 
	$style_check(_, Old).

list_undefined_ :-
	predicate_property(Module:Head, undefined), 
	\+ predicate_property(Module:Head, imported_from(_)), 
	functor(Head, Functor, Arity), 
	\+ $in_library(Functor, Arity),
	\+ system_undefined(Module:Functor/Arity),
	write_undefined(Module:Functor/Arity), 
	fail.
list_undefined_.

system_undefined(user:prolog_trace_interception/4).

write_undefined(user:Name/Arity) :- !, 
	format('~w/~w~n', [Name, Arity]).
write_undefined(Module:Name/Arity) :-
	format('~w:~w/~w~n', [Module, Name, Arity]).

%	list_autoload/0
%	Show predicates that need be linked via the autoload mechanism

list_autoload :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	please(autoload, OldAutoLoad, off),
	list_autoload_, 
	please(autoload, _, OldAutoLoad),
	$style_check(_, Old).
	
list_autoload_ :-
	predicate_property(Module:Head, undefined), 
	\+ predicate_property(Module:Head, imported_from(_)), 
	functor(Head, Functor, Arity), 
	$in_library(Functor, Arity),
	show_library(Module, Functor, Arity), 
	fail.
list_autoload_.

show_library(Module, Name, Arity) :-
	$find_library(Module, Name, Arity, _LoadModule, Library),
	(   Module == user
	->  format('~w/~w~t~30|~w~n', [Name, Arity, Library])
	;   format('~w:~w/~w~t~30|~w~n', [Module, Name, Arity, Library])
	).

%	list_redefined/0
%	Show redefined system predicates

list_redefined :-
	$style_check(Old, Old), 
	style_check(+dollar), 
	list_redefined_, 
	$style_check(_, Old).
	
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
	    functor(Head, Functor, Arity)
	->  show_redefined(Module, Super, Functor, Arity)
	),
	fail.
list_redefined_.

show_redefined(user, system, F, A) :- !,
	format('system predicate ~w/~w has been redefined globally.~n',
								[F, A]).
show_redefined(M, system, F, A) :-
	format('system predicate ~w/~w has been redefined in module ~w.~n',
								[F, A, M]).
show_redefined(M, user, F, A) :- !,
	format('global predicate ~w/~w has been redefined in module ~w.~n',
								[F, A, M]).

