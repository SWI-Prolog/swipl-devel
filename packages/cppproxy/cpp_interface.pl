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

:- module(cpp_interface,
	  [ current_cpp_callable/1,	% :Goal
	    current_cpp_callable/3,	% ?Module, ?Goal, ?Options
	    current_cpp_type/3,		% ?Module, ?CPPClass, ?Functor

	    (cpp_callable)/1,		% Define for errors
	    (cpp_type)/1,			% Define for errors

	    op(1199, fx, cpp_type),
	    op(1015, fx, cpp_callable),
	    op(100,  xf, non_det),
	    op(100,  xf, det)
	  ]).

:- meta_predicate
	current_cpp_callable(:).

%	:- cpp_callable Goal1, Goal2, ...
%	
%	Declare Goal1, ... to  be  callable   from  C++.  Callable goals
%	consist of a head, specifying the argument types and optional an
%	" = Options", where  Options  is   a  list  of  options. Defined
%	options are:
%	
%		* one
%		Predicate always succeeds.  This is the default.  Such
%		predicates are mapped to void C++ functions.
%
%		* zero_or_one
%		Predicate succeeds or fails.
%
%		* zero_or_more
%		Predicate is non-deterministic.
%		
%		* as(Name)
%		Name to give to the C++ function or Query class.

expand_callable((A,B)) --> !,
	expand_callable(A),
	expand_callable(B).
expand_callable(Head = Attrs) -->
	[ 'cpp callable'(Head, Attrs)
	].
expand_callable(Head) -->
	[ 'cpp callable'(Head, [one])
	].

expand_type((A,B)) --> !,
	expand_type(A),
	expand_type(B).
expand_type(CPP=Prolog) -->
	[ 'cpp type'(CPP, Prolog)
	].


cpp_callable(_) :-
	throw(error(context_error(directive), _)).
cpp_type(_) :-
	throw(error(context_error(directive), _)).

:- multifile
	user:term_expansion/2.

user:term_expansion((:- cpp_callable Spec), Clauses) :-
	phrase(expand_callable(Spec), Clauses).
user:term_expansion((:- cpp_type Spec), Clauses) :-
	phrase(expand_type(Spec), Clauses).

current_cpp_callable(Spec) :-
	strip_module(Spec, Module, Goal),
	current_cpp_callable(Module, Goal, _).
current_cpp_callable(Module, Goal, Attributes) :-
	predicate_property(Module:'cpp callable'(_,_), interpreted),
	Module:'cpp callable'(Goal, Attributes).

current_cpp_type(Module, CPP, Prolog) :-
	predicate_property(Module:'cpp type'(_,_), interpreted),
	Module:'cpp type'(CPP, Prolog).
