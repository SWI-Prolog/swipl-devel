/*  $Id$

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

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

 
:- module(ordering,
	[
	    combine/3,
	    ordering/1,
	    arrangement/2
	]).
:- use_module(class,
	[
	    class_get_clp/2,
	    class_get_prio/2,
	    class_put_prio/2
	]).
:- use_module(itf,
	[
	    clp_type/2
	]).
:- use_module(library(ugraphs),
	[
	    add_edges/3,
	    add_vertices/3,
	    top_sort/2,
	    ugraph_union/3
	]).
:- use_module(library(lists),
	[
	    append/3
	]).

ordering(X) :-
	var(X),
	!,
	fail.
ordering(A>B) :-
	!,
	ordering(B<A).
ordering(A<B) :-
	join_class([A,B],Class),
	class_get_prio(Class,Ga),
	!,
	add_edges([],[A-B],Gb),
	combine(Ga,Gb,Gc),
	class_put_prio(Class,Gc).
ordering(Pb) :-
	Pb = [_|Xs],
	join_class(Pb,Class),
	class_get_prio(Class,Ga),
	!,
	(   Xs = [],
	    add_vertices([],Pb,Gb)
	;   Xs=[_|_],
	    gen_edges(Pb,Es,[]),
	    add_edges([],Es,Gb)
	),
	combine(Ga,Gb,Gc),
	class_put_prio(Class,Gc).
ordering(_).

arrangement(Class,Arr) :-
	class_get_prio(Class,G),
	normalize(G,Gn),
	top_sort(Gn,Arr),
	!.
arrangement(_,_) :- throw(unsatisfiable_ordering).

join_class([],_).
join_class([X|Xs],Class) :-
	(   var(X)
	->  clp_type(X,CLP),
	    (   CLP = clpr
	    ->  bv_r:var_intern(X,Class)
	    ;   bv_q:var_intern(X,Class)
	    )
	;   true
	),
	join_class(Xs,Class).

% combine(Ga,Gb,Gc)
%
% Combines the vertices of Ga and Gb into Gc.

combine(Ga,Gb,Gc) :-
	normalize(Ga,Gan),
	normalize(Gb,Gbn),
	ugraph_union(Gan,Gbn,Gc).

%
% both Ga and Gb might have their internal ordering invalidated
% because of bindings and aliasings
% 

normalize([],[]) :- !.
normalize(G,Gsgn) :-
	G = [_|_],
	keysort(G,Gs),	% sort vertices on key
	group(Gs,Gsg),	% concatenate vertices with the same key
	normalize_vertices(Gsg,Gsgn).	% normalize

normalize_vertices([],[]).
normalize_vertices([X-Xnb|Xs],Res) :-
	(   normalize_vertex(X,Xnb,Xnorm)
	->  Res = [Xnorm|Xsn],
	    normalize_vertices(Xs,Xsn)
	;   normalize_vertices(Xs,Res)
	).

% normalize_vertex(X,Nbs,X-Nbss)
%
% Normalizes a vertex X-Nbs into X-Nbss by sorting Nbs, removing duplicates (also of X)
% and removing non-vars. 

normalize_vertex(X,Nbs,X-Nbsss) :-
	var(X),
	sort(Nbs,Nbss),
	strip_nonvar(Nbss,X,Nbsss).

% strip_nonvar(Nbs,X,Res)
%
% Turns vertext X-Nbs into X-Res by removing occurrences of X from Nbs and removing
% non-vars. This to normalize after bindings have occurred. See also normalize_vertex/3.

strip_nonvar([],_,[]).
strip_nonvar([X|Xs],Y,Res) :-
	(   X==Y % duplicate of Y
	->  strip_nonvar(Xs,Y,Res)
	;   var(X) % var: keep
	->  Res = [X|Stripped],
	    strip_nonvar(Xs,Y,Stripped)
	;   % nonvar: remove
	    nonvar(X),
	    Res = []	% because Vars<anything
	).

gen_edges([]) --> [].
gen_edges([X|Xs]) -->
	gen_edges(Xs,X),
	gen_edges(Xs).

gen_edges([],_) --> [].
gen_edges([Y|Ys],X) -->
	[X-Y],
	gen_edges(Ys,X).

% group(Vert,Res)
%
% Concatenates vertices with the same key.

group([],[]).
group([K-Kl|Ks],Res) :-
	group(Ks,K,Kl,Res).

group([],K,Kl,[K-Kl]).
group([L-Ll|Ls],K,Kl,Res) :-
	(   K==L
	->  append(Kl,Ll,KLl),
	    group(Ls,K,KLl,Res)
	;   Res = [K-Kl|Tail],
	    group(Ls,L,Ll,Tail)
	).
