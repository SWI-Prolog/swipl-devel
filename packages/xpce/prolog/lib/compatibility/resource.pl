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

:- module(pce_resource_compatibility, []).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module deals with backward compatibility   for packages written for
XPCE  versions  upto  4.10.1   (ProWindows    3.2),   using   `resource'
declaractions.

Now, resources are defined to be data associated with an application. In
the old version, resources refered to class-variables and settings.

This code deals with the necessary conversions,   and  should get 99% of
the old code working without modifications.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       METHODS		*
		 *******************************/

:- pce_extend_class(object).

resource_value(O, Name:name, RV:any) :<-
	"COMPATIBILITY: <-class_variable_value"::
	get(O, class_variable_value, Name, RV).

obtain_resources(O) :->
	"COMPATIBILITY: ->obtain_class_variables"::
	send(O, obtain_class_variables).

:- pce_end_class.

:- pce_extend_class(class).

resource(C, Name:name, Resource) :<-
	"COMPATIBILITY: return <-class_variable"::
	get(C, class_variable, Name, Resource).

:- pce_end_class.


		 /*******************************
		 *      CLASS DECLARATIONS	*
		 *******************************/

:- dynamic
	user:pce_pre_expansion_hook.
:- multifile
	user:pce_pre_expansion_hook.

user:pce_pre_expansion_hook(resource(Name, Type, Value),
			    class_variable(Name, Type, NewVal)) :-
	pce_compiling(_Class, Path),
	source_location(Path, _),
	pce_resource_compatibility:rc_convert_value(Value, Type, NewVal).
user:pce_pre_expansion_hook(resource(Name, Type, Value, Summary),
			    class_variable(Name, Type, NewVal, Summary)) :-
	pce_compiling(_Class, Path),
	source_location(Path, _),
	pce_resource_compatibility:rc_convert_value(Value, Type, NewVal).

rc_convert_value(Value, Type, NewVal) :-
	convert_value(Value, Type, NewVal), !,
	pce_warn(compatibility(resource(Value, NewVal))).
rc_convert_value(Value, _, Value).

convert_value(Value, _chain, NewVal) :-		% list --> chain(members)
	is_list(Value), !,
	NewVal =.. [chain|Value].
convert_value(Value, _Type, NewVal) :-		% '@name' --> @name
	atom(Value),
	term_to_atom(NewVal, Value),
	NewVal = @Name,
	atom(Name), !.
convert_value(_, Type, _) :-			% textual types
	atomic_type(Type), !,
	fail.
convert_value(Value, Type, NewVal) :-		% Other values
	atom(Value),
	term_to_atom(Term, Value),
	\+ atom(Term), !,
	(   convert_value(Term, Type, NewVal)
	->  true
	;   NewVal = Term
	).

atomic_type(string).
atomic_type(name).
atomic_type(char_array).
atomic_type(geometry).
atomic_type([T]) :-
	atomic_type(T).
atomic_type(*(T)) :-
	atomic_type(T).
