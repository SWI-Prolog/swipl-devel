/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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

:- module(terms,
	  [ term_hash/2,		% @Term, -HashKey
	    term_hash/4,		% @Term, +Depth, +Range, -HashKey
	    term_variables/2,		% @Term, -Variables
	    term_variables/3,		% @Term, -Variables, +Tail
	    variant/2,			% @Term1, @Term2
	    subsumes/2,			% +Generic, @Specific
	    subsumes_chk/2,		% +Generic, @Specific
	    cyclic_term/1,		% @Term
	    acyclic_term/1		% @Term
	  ]).

/** <module> Term manipulation

Compatibility library for term manipulation   predcates. Most predicates
in this library are provided as SWI-Prolog built-ins.

@compat	YAP, SICStus, Quintus.  Not all versions of this library define
	exactly the same set of predicates, but defined predicates are
	compatible.
*/

%%	variant(@Term1, @Term2) is semidet.
%
%	Same as SWI-Prolog =|Term1 =@= Term2|=.

variant(X, Y) :-
	X =@= Y.

