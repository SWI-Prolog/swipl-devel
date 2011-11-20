/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam
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

:- module(swi_system_utilities,
	[ lock_predicate/1,
	  unlock_predicate/1,
	  system_mode/1,
	  system_module/0
	]).
:- use_module(library(error)).

/** <module> System utilities

This module provides some tools to deal with system predicates. System
predicates cannot be traced or redefined.

@deprecated	Use :- set_prolog_flag(generate_debug_info, false) to
		hide predicate internals from the tracer.
@tbd		Move this functionality to prolog flags.
*/

%%	system_mode(+Boolean) is det.
%
%	Switch the system into system or user mode.  When in system mode,
%	system predicates loose most of their special properties, so it
%	becomes possible to trace and even redefine them.
%
%	@deprecated  New code should use the prolog flag =access_level=.

system_mode(Val) :-
	must_be(boolean, Val),
	(   Val == true
	->  set_prolog_flag(access_level, system)
	;   set_prolog_flag(access_level, user)
	).

%%	system_module
%
%	Any predicate defined after this declaraction   uptil the end of
%	the file will become a system   predicate. Normally invoked by a
%	directive immediately following the module declaration.

system_module :-
	set_prolog_flag(generate_debug_info, false).

:- meta_predicate
	lock_predicate(:),
	unlock_predicate(:).

%%	lock_predicate(+PredInd)
%
%	Transform a predicate into a system predicate.

lock_predicate(PredInd) :-
	'$set_predicate_attribute'(PredInd, system, 1).

%%	unlock_predicate(+PredInd)
%
%	Transform a system predicate into a normal system predicate.

unlock_predicate(PredInd) :-
	'$set_predicate_attribute'(PredInd, system, 0).
