/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
