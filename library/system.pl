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

:- module(swi_system_utilities,
	[ lock_predicate/2
	, unlock_predicate/2
	, system_mode/1
	, system_module/0
	]).

:- style_check(+dollar).

%	system_mode(+OnOff)
%	Switch the system into system or user mode.  When in system mode,
%	system predicates loose most of their special properties, so it
%	becomes possible to trace and even redefine them.  Use the latter
%	with care as the system predicates call one another.  This should
%	once be fixed by defining all of them in a module ($system), so
%	the user can savely remove them from module user.

system_mode(X) :-
	var(X), !,
	(   style_check(?dollar)
	->  X = on
	;   X = off
	).
system_mode(on) :-
	style_check(+dollar).
system_mode(off) :-
	style_check(-dollar).

%	system_module
%	Any predicate defined after this declaraction uptill the end of
%	the file will become a system predicate. Normally invoked by a
%	directive immediately following the module declaration.

system_module :-
	system_mode(on).

:- module_transparent
	lock_predicate/2,
	unlock_predicate/2.

%	lock_predicate(+Name, Arity)
%	Transform a predicate into a system predicate. 

lock_predicate(Spec, Arity) :-
	$strip_module(Spec, Module, Name),
	functor(Head, Name, Arity ),
	$set_predicate_attribute(Module:Head, system, 1).

%	unlock_predicate(+Name, Arity)
%	Transform a system predicate into a normal system predicate.

unlock_predicate(Spec, Arity) :-
	$strip_module(Spec, Module, Name),
	functor(Head, Name, Arity ),
	$set_predicate_attribute(Module:Head, system, 0).
