/*  $Id$

    Part of SWI-Prolog

    Author:        Tom Schrijvers, K.U.Leuven
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U.Leuven

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This module implements the dif/2 constraint. It constraint two terms to be
% not identical.
%
%	Author: 	Tom Schrijvers, K.U.Leuven
% 	E-mail: 	Tom.Schrijvers@cs.kuleuven.ac.be
%	Copyright:	2003-2004, K.U.Leuven
%
% Update 7/3/2004:
%   Now uses unifyable/3. It enables dif/2 to work with infinite terms.
% Update 11/3/2004:
%   Cleaned up code. Now uses just one or node for every call to dif/2.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(dif,[dif/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
dif(X,Y) :-
	dif_c_c(X,Y,_).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% types of attributes?
% 	vardif: X is a variable
%	node(Parent,Children,Variables,Counter)

dif_c_c(X,Y,OrNode) :-
	( unifyable(X,Y,Unifier) ->
		( Unifier == [] ->
			or_one_fail(OrNode)
		;
			dif_c_c_l(Unifier,OrNode)
		)
	;
		or_succeed(OrNode)
	).

dif_c_c_l(Unifier,OrNode) :-
	length(Unifier,N),
	extend_ornode(OrNode,N,List,Tail),
	dif_c_c_l_aux(Unifier,OrNode,List,Tail).	

extend_ornode(OrNode,N,List,Vars) :-
	( get_attr(OrNode,dif,Attr) ->
		Attr = node(M,Vars),
		O is N + M - 1
	;
		O = N,
		Vars = []
	),
	put_attr(OrNode,dif,node(O,List)).
		
dif_c_c_l_aux([],_,List,List).
dif_c_c_l_aux([X=Y|Unifier],OrNode,List,Tail) :-
	List = [X=Y|Rest],
	add_ornode(X,Y,OrNode),
	dif_c_c_l_aux(Unifier,OrNode,Rest,Tail).	

add_ornode(X,Y,OrNode) :-
	add_ornode_var1(X,Y,OrNode),
	( var(Y) ->
		add_ornode_var2(X,Y,OrNode)
	;
		true
	).	

add_ornode_var1(X,Y,OrNode) :-
	( get_attr(X,dif,Attr) ->
		Attr = vardif(V1,V2),
		put_attr(X,dif,vardif([OrNode-Y|V1],V2))
	;
		put_attr(X,dif,vardif([OrNode-Y],[]))
	).

add_ornode_var2(X,Y,OrNode) :-
	( get_attr(Y,dif,Attr) ->
		Attr = vardif(V1,V2),
		put_attr(Y,dif,vardif(V1,[OrNode-X|V2]))
	;
		put_attr(Y,dif,vardif([],[OrNode-X]))
	).

attr_unify_hook(vardif(V1,V2),Other) :-
	( var(Other) ->
		reverse_lookups(V1,Other,OrNodes1,NV1),
		or_one_fails(OrNodes1),
		get_attr(Other,dif,OAttr),
		OAttr = vardif(OV1,OV2),
		reverse_lookups(OV1,Other,OrNodes2,NOV1),
		or_one_fails(OrNodes2),
		remove_obsolete(V2,Other,NV2),
		remove_obsolete(OV2,Other,NOV2),
		append(NV1,NOV1,CV1),
		append(NV2,NOV2,CV2),
		( CV1 == [], CV2 == [] ->
			del_attr(Other,dif)
		;
			put_attr(Other,dif,vardif(CV1,CV2))	
		)
	;
		verify_compounds(V1,Other),
		verify_compounds(V2,Other)	
	).

remove_obsolete([], _, []).
remove_obsolete([N-Y|T], X, L) :-
        (   Y==X ->
            remove_obsolete(T, X, L)
        ;   L=[N-Y|RT],
            remove_obsolete(T, X, RT)
        ).

reverse_lookups([],_,[],[]).
reverse_lookups([N-X|NXs],Value,Nodes,Rest) :-
	( X == Value ->
		Nodes = [N|RNodes],
		Rest = RRest
	;
		Nodes = RNodes,
		Rest = [N-X|RRest]
	),
	reverse_lookups(NXs,Value,RNodes,RRest).

verify_compounds([],_).
verify_compounds([Node-Y|Rest],X) :-
	( var(Y) ->
		true
	;
		dif_c_c(X,Y,Node)
	),
	verify_compounds(Rest,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
or_succeed(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,dif,Attr),
		Attr = node(_Counter,Pairs),
		del_attr(OrNode,dif),
		OrNode = (-),
		del_or_dif(Pairs)
	;
		true
	).

or_one_fails([]).
or_one_fails([N|Ns]) :-
	or_one_fail(N),
	or_one_fails(Ns).

or_one_fail(OrNode) :-
	( attvar(OrNode) ->
		get_attr(OrNode,dif,Attr),
		Attr = node(Counter,Pairs),
		NCounter is Counter - 1,
		( NCounter == 0 ->
			fail
		;
			put_attr(OrNode,dif,node(NCounter,Pairs))
		)
	;
		fail	
	).

del_or_dif([]).
del_or_dif([X=Y|Xs]) :-
	cleanup_dead_nodes(X),
	cleanup_dead_nodes(Y),
	del_or_dif(Xs).

cleanup_dead_nodes(X) :-
 	( attvar(X) ->
 		get_attr(X,dif,Attr),
		Attr = vardif(V1,V2),
		filter_dead_ors(V1,NV1),
		filter_dead_ors(V2,NV2),
		( NV1 == [], NV2 == [] ->
			del_attr(X,dif) 
		;
			put_attr(X,dif,vardif(NV1,NV2))	
		)
	;
		true
	).

filter_dead_ors([],[]).
filter_dead_ors([Or-Y|Rest],List) :-
	( var(Or) ->
		List = [Or-Y|NRest]
	;
		List = NRest
	),
	filter_dead_ors(Rest,NRest).

attr_portray_hook(vardif(V1,V2),_) :-
	snd_of_pairs(V1,VV1),
	snd_of_pairs(V2,VV2),
	append(VV1,VV2,VV),
	format('~w',[dif(VV)]).

% from hProlog's pairlist module
snd_of_pairs([],[]).
snd_of_pairs([_-Y|XYs],[Y|Ys]) :-
	snd_of_pairs(XYs,Ys).
