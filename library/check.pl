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
	current_prolog_flag(autoload, OldAutoLoad),
	set_prolog_flag(autoload, false),
	list_autoload_, 
	set_prolog_flag(autoload, OldAutoLoad),
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

