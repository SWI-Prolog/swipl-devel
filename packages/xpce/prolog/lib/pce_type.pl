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


