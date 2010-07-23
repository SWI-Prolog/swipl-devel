/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, University of Amsterdam, VU University Amsterdam

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
	    acyclic_term/1,		% @Term
	    term_subsumer/3		% +Special1, +Special2, -General
	  ]).
:- use_module(library(rbtrees)).

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

%%	subsumbes_chk(@Generic, @Specific)
%
%	True if Generic can be made equivalent to Specific without
%	changing Specific.
%
%	@deprecated Replace by subsumbes_term/2.

subsumes_chk(Generic, Specific) :-
	subsumbes_term(Generic, Specific).

%%	subsumbes(+Generic, @Specific)
%
%	True  if  Generic  is  unified   to  Specific  without  changing
%	Specific.
%
%	@deprecated It turns out that calls to this predicate almost
%	always should have used subsumbes_term/2.  Also the name is
%	misleading.  In case this is really needed, one is adviced to
%	follow subsumbes_term/2 with an explicit unification.

subsumes(Generic, Specific) :-
	subsumbes_term(Generic, Specific),
	Generic = Specific.

%%	term_subsumer(+Special1, +Special2, -General) is det.
%
%	General is the most specific term   that  is a generalisation of
%	Special1 and Special2. The  implementation   can  handle  cyclic
%	terms.
%
%	@compat SICStus
%	@author Inspired by LOGIC.PRO by Stephen Muggleton

%	It has been rewritten by  Jan   Wielemaker  to use the YAP-based
%	red-black-trees as mapping rather than flat  lists and use arg/3
%	to map compound terms rather than univ and lists.

term_subsumer(S1, S2, G) :-
	cyclic_term(S1),
	cyclic_term(S2), !,
	rb_empty(Map),
	lgg_safe(S1, S2, G, Map, _).
term_subsumer(S1, S2, G) :-
	rb_empty(Map),
	lgg(S1, S2, G, Map, _).

lgg(S1, S2, G, Map0, Map) :-
	(   S1 == S2
	->  G = S1,
	    Map = Map0
	;   compound(S1),
	    compound(S2),
	    functor(S1, Name, Arity),
	    functor(S2, Name, Arity)
	->  functor(G, Name, Arity),
	    lgg(0, Arity, S1, S2, G, Map0, Map)
	;   rb_lookup(S1+S2, G0, Map0)
	->  G = G0,
	    Map	= Map0
	;   rb_insert(Map0, S1+S2, G, Map)
	).

lgg(Arity, Arity, _, _, _, Map, Map) :- !.
lgg(I0, Arity, S1, S2, G, Map0, Map) :-
	I is I0 + 1,
	arg(I, S1, Sa1),
	arg(I, S2, Sa2),
	arg(I, G, Ga),
	lgg(Sa1, Sa2, Ga, Map0, Map1),
	lgg(I, Arity, S1, S2, G, Map1, Map).


%%	lgg_safe(+S1, +S2, -G, +Map0, -Map) is det.
%
%	Cycle-safe version of the  above.  The   difference  is  that we
%	insert compounds into the mapping table   and  check the mapping
%	table before going into a compound.

lgg_safe(S1, S2, G, Map0, Map) :-
	(   S1 == S2
	->  G = S1,
	    Map = Map0
	;   rb_lookup(S1+S2, G0, Map0)
	->  G = G0,
	    Map	= Map0
	;   compound(S1),
	    compound(S2),
	    functor(S1, Name, Arity),
	    functor(S2, Name, Arity)
	->  functor(G, Name, Arity),
	    rb_insert(Map0, S1+S2, G, Map1),
	    lgg_safe(0, Arity, S1, S2, G, Map1, Map)
	;   rb_insert(Map0, S1+S2, G, Map)
	).

lgg_safe(Arity, Arity, _, _, _, Map, Map) :- !.
lgg_safe(I0, Arity, S1, S2, G, Map0, Map) :-
	I is I0 + 1,
	arg(I, S1, Sa1),
	arg(I, S2, Sa2),
	arg(I, G, Ga),
	lgg_safe(Sa1, Sa2, Ga, Map0, Map1),
	lgg_safe(I, Arity, S1, S2, G, Map1, Map).
