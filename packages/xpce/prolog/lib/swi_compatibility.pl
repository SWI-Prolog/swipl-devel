/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

:- module(pce_compatibility_layer,
	  [ auto_call/1,
	    callable_predicate/1,
	    modified_since_last_loaded/1,
	    pce_error/1,
	    pce_warn/1,
	    pce_info/1
	  ]).

:- module_transparent
	auto_call/1,
	callable_predicate/1.

auto_call(G) :-
	G.


		 /*******************************
		 *      DIALOG EDITOR SUPPORT	*
		 *******************************/

%	callable_predicate(:Head)
%
%	Succeeds if Head can be called without raising an exception for
%	an undefined predicate

callable_predicate(Spec) :-
	strip_module(Spec, M, Head),
	default_module(M, Def),
	current_predicate(_, Def:Head), !.
callable_predicate(Spec) :-
	strip_module(Spec, _, Head),
	functor(Head, Name, Arity),
	'$in_library'(Name, Arity).

%	modified_since_last_loaded(Path)
%	True is file has been modified since the last time it was loaded.

modified_since_last_loaded(File) :-
	'$time_source_file'(File, LoadTime), !,
	time_file(File, Modified),
	Modified @> LoadTime.
modified_since_last_loaded(InFile) :-
	'$time_source_file'(File, LoadTime),
	same_file(InFile, File), !,
	time_file(File, Modified),
	Modified @> LoadTime.


		 /*******************************
		 *	     MESSAGES		*
		 *******************************/

:- consult(library('english/pce_messages')).

:- multifile
	prolog:message/3.

prolog:message(Spec) -->
	pce_message(Spec).
prolog:message(context_error(Goal, Context, What)) -->
	[ '~w: ~w '-[Goal, What] ],
	pce_message_context(Context).
prolog:message(type_error(Goal, ArgN, Type, _Value)) -->
	[ '~w: argument ~w must be a ~w'-[Goal, ArgN, Type], nl ].

pce_error(Term) :-
	print_message(error, Term).

pce_warn(Term) :-
	print_message(warning, Term).

pce_info(Term) :-
	print_message(informational, Term).


