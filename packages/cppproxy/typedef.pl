/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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

:- module(typedef,
	  [ current_type/2,		% :Type, -Expanded
	    (type)/1,			% Catch calls

	    op(1199, fx, type)
	  ]).

:- meta_predicate
	current_type(:, -).

		 /*******************************
		 *	    EXPANSION		*
		 *******************************/

type(_) :-
	throw(error(context_error(directive), _)).

expand_type_term(:- type TypeDecl, Clauses) :-
	phrase(type_clauses(TypeDecl), Clauses).

type_clauses(Name=Alias) -->
	[ 'type alias'(Name, Alias) ].
type_clauses(Name->Decl) -->
	{ or_to_list(Decl, List)
	},
	[ 'type def'(Name, List)
	].

or_to_list(A|B0, [A|B]) :- !,
	or_to_list(B0, B).
or_to_list(A, [A]).

:- multifile
	user:term_expansion/2.

user:term_expansion((:- type TypeDecl), Clauses) :-
	expand_type_term((:- type TypeDecl), Clauses).


		 /*******************************
		 *	      QUERY		*
		 *******************************/

%	current_type(:Name, -Expanded)
%	
%	Do alias expansion of the  type   Name  and  return the expanded
%	types. Expanded is a list of allowed types (polymorphism).

current_type(NameSpec, Expanded) :-
	strip_module(NameSpec, Module, Name),
	current_type(Module, Name, Expanded).

current_type(Module, Name, Expanded) :-
	current_predicate(_, Module:'type alias'(_, _)),
	Module:'type alias'(Name, Alias), !,
	current_type(Module, Alias, Expanded).
current_type(_, Primitive, [Primitive]) :-
	primitive(Primitive), !.
current_type(Module, Name, Expanded) :-
	Module:'type def'(Name, Expanded).


%	primitive(?Type)
%	
%	True if type is a primitive type

primitive(integer).
primitive(atom).
primitive(float).
primitive(var).
