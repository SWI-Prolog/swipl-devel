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

:- module(require, [require/1]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines the  Prolog  predicate   require/1  which  allows  for
portable loading of libraries on  systems   with  a  Quintus-like module
system.  require/1 are part of both  SWI-Prolog and SICStus Prolog.  For
other Prologs this module should be ported to the Prolog system and made
part of the module `pce', so any PCE/Prolog program can start with:

:- module(foobar,
	  [ foobar/1,
	    ...
	  ]).
:- use_module(library(pce)).
:- require([ member/1
	   , forall/2
	   , send_list/3
	   ]).

without having to worry about the available system predicates, autoload
libraries, library structure, etc.

This software was originally written for the SWI-Prolog autoloader.

Auto_call/1 added for xpce 4.8.6
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- meta_predicate
	require(:).


:- use_module(pce_utils, [strip_module/3]).


require(Spec) :-
	strip_module(Spec, Module, Predicates),
	require(Module, Predicates).


require(_, []) :- !.
require(Module, [H|T]) :- !,
	require(Module, H),
	require(Module, T).
require(_Module, Name/Arity) :-		% a builtin
	functor(Head, Name, Arity),
	predicate_property(Head, built_in).
require(Module, Name/Arity) :-		% already defined
	functor(Head, Name, Arity),
	current_predicate(Name, Module:Head), !.
require(Module, Name/Arity) :-		% load from library
	load_library_index,
	functor(Head, Name, Arity),
	library_index(Head, _, File),
	Module:use_module(library(File), [Name/Arity]).
require(_Module, Name/Arity) :-
	print_message(error, required_predicate_not_found(Name,Arity)).

te_require_list(Specs, UseModules) :-
	load_library_index,
	map_requirements(Specs, Libs),
	combine_use_modules(Libs, UseModules).

map_requirements([], []) :- !.
map_requirements([Name/Arity|Tail], List) :-
        functor(Head, Name, Arity),
	(   predicate_property(Head, built_in)
	->  List = ListRest
	;   library_index(Head, _, File)
	->  List = [lib(File, Head)|ListRest]
	;   print_message(warning, required_predicate_not_found(Name,Arity)),
	    List = ListRest
	),
	map_requirements(Tail,ListRest).

combine_use_modules([], []).
combine_use_modules([lib(Lib, Term)|Rest],
		    [(:- use_module(library(Lib), [Name/Ar|Ps])), RestDecl]) :-
	functor(Term, Name, Ar),
	collect_for_lib(Rest, Lib, Ps, [], R0),
	combine_use_modules(R0, RestDecl).
				  
collect_for_lib([], _, [], R, R).
collect_for_lib([lib(Lib, Term)|L], Lib, [Name/Arity|RH], R0, R) :- !,
	functor(Term, Name, Arity),
	collect_for_lib(L, Lib, RH, R0, R).
collect_for_lib([LT|L], Lib, RH, R0, R) :-
	collect_for_lib(L, Lib, RH, [LT|R0], R).


		/********************************
		*           LOAD INDEX		*
		********************************/

:- dynamic
	library_index/3.			% Head x Module x Path

load_library_index :-
	library_index(_, _, _), !.		% loaded
load_library_index :-
	absolute_file_name(library('QPINDEX.pl'),
			   [ access(read),
			     file_errors(fail)
			   ], Index),
	!,
        read_index(Index),
	(   absolute_file_name(library('INDEX.pl'),
			       [ access(read),
				 file_errors(fail),
				 solutions(all)
			       ], LibIndex),
	    read_index(LibIndex),
	    fail
	;   true
	).
load_library_index :-
	print_message(warning, index_not_found(library('QPINDEX.pl'))).

read_index(Index) :-
	seeing(Old), see(Index),
	repeat,
	    read(Term),
	    (   Term == end_of_file
	    ->  !
	    ;   assert_index(Term, Index),
	        fail
	    ),
	seen, see(Old),
	print_message(informational, loaded_library_index(Index)).

assert_index(index(Name, Arity, Module, File), _Index) :- !,
	functor(Head, Name, Arity),
	assertz(library_index(Head, Module, File)).
assert_index(Term, Index) :-
	print_message(warning, illegal_term_in_index(Term,Index)).

