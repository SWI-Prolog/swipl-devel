/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

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

:- module(listmap,
	[
		listmap_empty/1,
		listmap_lookup/3,
		listmap_insert/4,
		listmap_remove/3,
		listmap_merge/5
	]).

listmap_empty([]).

listmap_lookup([K-V|R],Key,Q) :-
	( Key == K ->
		Q = V
	;
		Key @> K,
		listmap_lookup(R,Key,Q)
	).

listmap_insert([],Key,Value,[Key-Value]).
listmap_insert([P|R],Key,Value,ML) :-
	P = K-_,
	compare(C,Key,K),
	( C == (=) ->
		ML = [K-Value|R]
	; C == (<) ->
		ML = [Key-Value,P|R]
	;
		ML = [P|Tail],
		listmap_insert(R,Key,Value,Tail)
	).

listmap_merge(ML1,ML2,F,G,ML) :-
	( ML1 == [] ->
		ML = ML2
	; ML2 == [] ->
		ML = ML1
	;
		ML1 = [P1|R1], P1 = K1-V1,
		ML2 = [P2|R2], P2 = K2-V2,
		compare(C,K1,K2),
		( C == (=) ->
			Call =.. [F,V1,V2,NV],
			call(Call),
			ML = [K1-NV|Tail],
			listmap_merge(R1,R2,F,G,Tail)
		; C == (<) ->
			Call =.. [G,V1,NV],
			call(Call),
			ML = [K1-NV|Tail],
			listmap_merge(R1,ML2,F,G,Tail)
		;
			Call =.. [G,V2,NV],
			call(Call),
			ML = [K2-NV|Tail],
			listmap_merge(ML1,R2,F,G,Tail)
		)
	).
		
	
listmap_remove([],_,[]).
listmap_remove([P|R],Key,NLM) :-
	P = K-_,
	compare(C,Key,K),
	( C == (=) ->
		NLM = R
	; C == (<) ->
		NLM = [P|R]
	;
		NLM = [P|Tail],
		listmap_remove(R,Key,Tail)
	).
		
		
