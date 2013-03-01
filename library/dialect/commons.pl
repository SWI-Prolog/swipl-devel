/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009-2013, University of Amsterdam
			      VU University Amsterdam

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

:- module(commons,
	  [ feature/1				% +Feature
	  ]).
:- use_module(library(error)).

/** <module> Implement Prolog Commons infrastructure
*/

:- multifile
	feature/1.

%%	new_declaration(+PredicateIndicator)
%
%	Directive that tells the system that PredicateIndicator can be
%	used as a directive.

user:term_expansion(new_declaration(Name/Arity),
	       '$directive'(Head)) :-
	functor(Head, Name, Arity).
user:term_expansion((:- Directive), []) :-
	current_predicate('$directive'/1),
	'$directive'(Directive).
user:term_expansion((:- module(Name, Public, Import)),
		    [ (:- module(Name, Public))
		    | ImportsDecls
		    ]) :-
	maplist(import_decl, Import, ImportsDecls).

import_decl(Name,
	    use_module(library(dialect/Name))).


%%	feature(+Feature) is semidet.
%
%	Provide the condition for :- if(feature(...)).

feature(Var) :-
	var(Var), !,
	instantiation_error(Var).
feature(implementation_defined(PI)) :-
	current_predicate(PI).

