/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, University of Amsterdam

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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(ciao,
	  [
	  ]).

:- multifile
	system:goal_expansion/2.
:- multifile
	system:term_expansion/2.

:- multifile
	declaration/1.			% Head

system:term_expansion((:- module(Name, Public, Packages)),
		      [ (:- module(Name, Public))
		      |	Directives
		      ]) :-
	maplist(package_directive, Packages, Directives).
system:term_expansion((:- use_package(Name)),
		      (:- include(library(Name)))).
system:term_expansion((:- new_declaration(Name/Arity)),
		      ciao:declaration(Head)) :-
	functor(Head, Name, Arity).
system:term_expansion((:- Decl), Exp) :-
	declaration(Decl),
	(   functor(Decl, Name, Arity),
	    prolog_load_context(module, Module),
	    current_predicate(Module:Name/Arity)
	->  Exp = (:- Decl)
	;   Exp = []
	).

package_directive(Package, Directive) :-
	expand_term((:- use_package(Package)), Directive).


		 /*******************************
		 *	    LIBRARY SETUP	*
		 *******************************/

%%	push_ciao_library
%
%	Pushes searching for dialect/ciao in   front of every library
%	directory that contains such as sub-directory.

push_ciao_library :-
	(   absolute_file_name(library(dialect/ciao), Dir,
			       [ file_type(directory),
				 access(read),
				 solutions(all),
				 file_errors(fail)
			       ]),
	    asserta((user:file_search_path(library, Dir) :-
		    prolog_load_context(dialect, ciao))),
	    fail
	;   true
	).


:- push_ciao_library.
