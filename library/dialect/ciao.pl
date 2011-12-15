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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

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
:- use_module('../error').
:- use_module('../apply').
:- use_module('../debug').

/** <module> Ciao Prolog compatibility module

This module sets up support for loading   Ciao Prolog modules that start
with a :- module(Name, Exports,   Packages) directive. Upon encountering
this directive, it is rewritten into   a  SWI-Prolog module declaration,
followed by a series of directives to setup Ciao compatibility.

Typical usage for loading Ciao code is:

    ==
    :- use_module(library(dialect/ciao)).
    ==

@tbd	Create much more of the Ciao infrastructure.
*/

:- multifile
	system:goal_expansion/2,
	system:term_expansion/2,
	user:file_search_path/2.


		 /*******************************
		 *	       PATHS		*
		 *******************************/

user:file_search_path(engine, library(dialect/ciao/engine)).


		 /*******************************
		 *    MODULES & DECLARATIONS	*
		 *******************************/

:- multifile
	declaration/1.			% Head

system:term_expansion((:- module(Name, Public, Packages)),
		      [ (:- module(Name, Public)),
			(:- style_check(-atom)),
			(:- style_check(-singleton)),
			(:- expects_dialect(ciao))
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
system:term_expansion((:- meta_predicate(CiaoSpec)),
		      [ (:- meta_predicate(SWISpec))
		      | Wrappers
		      ]) :-
	prolog_load_context(dialect, ciao),
	(   phrase(map_metaspecs(CiaoSpec, SWISpec), Wrappers)
	->  true
	;   debug(ciao, 'Failed to translate ~q',
		  [(:- meta_predicate(CiaoSpec))]),
	    fail
	).

package_directive(Package, Directive) :-
	expand_term((:- use_package(Package)), Directive).

%%	map_metaspecs(+CiaoSpec, -SWISpec)// is det.
%
%	Map a Ciao meta-predicate to a SWI-Prolog one.
%
%	@see http://clip.dia.fi.upm.es/Software/Ciao/ciao_html/ciao_14.html

map_metaspecs(Var, _) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
map_metaspecs((A0,B0), (A,B)) --> !,
	map_metaspecs(A0, A),
	map_metaspecs(B0, B).
map_metaspecs(Head0, Head) -->
	{ \+ (arg(_, Head0, S), S==addmodule), !,
	  Head0 =.. [Name|Args0],
	  maplist(map_metaspec, Args0, Args),
	  Head =.. [Name|Args]
	}.
map_metaspecs(Head0, Head) -->
	{ functor(Head0, Name, Arity),
	  functor(HeadIn, Name, Arity),
	  HeadIn =.. [Name|ArgsIn],
	  Head0 =.. [Name|Args0],
	  map_mspec_list(Args0, Args, M, ArgsIn, ArgsOut),
	  Head =.. [Name|Args],
	  HeadOut =.. [Name|ArgsOut]
	},
	[ (:- module_transparent(Name/Arity)),
	  (HeadIn :- context_module(M), HeadOut)
	].

map_metaspec(Var, ?) :-
	var(Var), !.
map_metaspec(goal, 0).
map_metaspec(clause, :).
map_metaspec(fact, :).
map_metaspec(spec, :).
map_metaspec(pred(N), N).
map_metaspec(?, ?).
map_metaspec(+, +).
map_metaspec(-, -).

map_mspec_list([], [], _, [], []).
map_mspec_list([S0|TA0], [S|TA], M, [O|OT0], [O|OT]) :-
	map_metaspec(S0, S), !,
	map_mspec_list(TA0, TA, M, OT0, OT).
map_mspec_list([addmodule|TA0], TA, M, [O|OT0], [O,M|OT]) :- !,
	map_mspec_list(TA0, TA, M, OT0, OT).


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
