/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

:- module('$apply',
	  [ forall/2,			% :Goal, :Goal
	    apply/2			% :Goal, +ExtraArgs
	  ]).

:- meta_predicate
	forall(0,0),
	apply(:, +).

:- noprofile((forall/2, apply/2)).

%%	forall(+Condition, +Action)
%
%	True if Action if true for all variable bindings for which Condition
%	if true.

forall(Cond, Action) :-
	\+ (Cond, \+ Action).

%%	apply(:Goal, +ExtraArgs) is nondet.
%
%	Extend Goal with arguments from ExtraArgs and call it.
%
%	@deprecated	Almost all usage can be replaced by call/N.

apply(M:Name, Extra) :-
	atom(Name), !,
	compound_name_arguments(G, Name, Extra),
	call(M:G).
apply(M:Goal, Extra) :-
	compound(Goal), !,
	compound_name_arguments(Goal, Name, Args0),
	'$append'(Args0, Extra, Args),
	compound_name_arguments(G, Name, Args),
	call(M:G).
apply(_:Goal, _Extra) :-
	'$type_error'(callable, Goal).
