/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_type,
	  [ pce_define_type/2
	  ]).
:- use_module(library(pce)).

%       pce_define_type(+Name, +Type)
%
%       Create a type alias name, so we can write more readable code.
%	Typical examples make aliases for `name' (name cannot be subclassed),
%	alias for numeric and name-sets.  Here are some examples:
%
%	:- pce_define_type(rdf_resource, name).
%	:- pce_define_type(weekday,	{sunday,monday,tuesday,wednesday,
%					 thursday,friday,saturday}).
%	:- pce_define_type(natural,	'1..').

pce_define_type(Alias, Type) :-
        get(@types, member, Alias, TypeObj), !,
        (   get(TypeObj, kind, alias),
            get(TypeObj, context, Aliased),
            get(Aliased, name, Type)
        ->  true
        ;   throw(error(redefine(type, Alias), _))
        ).
pce_define_type(Alias, Type) :-
	(   object(Type)
	->  TheType = Type
	;   atom(Type)
	->  get(@pce, convert, Type, type, TheType)
	;   sformat(Atom, '~w', Type),
	    get(@pce, convert, Atom, type, TheType)
	),
        new(_, type(Alias, alias, TheType)).


