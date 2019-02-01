/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2019, CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(basics,
	  [ append/3, flatten/2, ith/3,
            length/2, member/2, memberchk/2, subset/2, subseq/3,
            reverse/2, select/3,

            for/3,                               % ?I,+B1,+B2)
            between/3,

            ground/1,
            copy_term/2,

            log_ith/3, log_ith_bound/3, log_ith_new/3, log_ith_to_list/2,
            logk_ith/4,

            comma_memberchk/2, abscomma_memberchk/2, comma_to_list/2,
            comma_length/2, comma_member/2, comma_append/3
	  ]).
:- use_module(library(lists)).

/** <module> XSB basics.P emulation

This module provides the XSB `basics`  module. The implementation either
simply uses SWI-Prolog built-ins and libraries or is copied from the XSB
file.

@license LGPLv2
*/

:- license(lgpl).

%!  for(?I,+B1,+B2)
%
%   Nondeterministically binds I to all  integer   values  from B1 to B2
%   inclusive. B1 and B2 must be integers, but either may be larger.

for(I, B1, B2) :-
    B2 >= B1,
    !,
    between(B1, B2, I).
for(I, B1, B2) :-
    End is B1 - B2,
    between(0, End, Diff),
    I is B1-Diff.

%!  ith(?Index, +List, ?Element)

ith(Index,List,Element) :-
    nth1(Index, List, Element).

subseq([],[],[]).
subseq([H|T],[H|S],C) :- subseq(T,S,C).
subseq([H|T],S,[H|C]) :- subseq(T,S,C).

log_ith(K,T,E) :-
	(integer(K)	% integer
	 ->	log_ith0(K,T,E,1)
	 ;	log_ith1(K,T,E,1)
	).

% K is bound
log_ith0(K,[L|R],E,N) :-
	(K < N
	 ->	bintree0(K,L,E,N)
	 ;	K1 is K-N,
		N2 is N+N,
		log_ith0(K1,R,E,N2)
	).

% First arg (K) is bound
bintree0(K,T,E,N) :-
	(N > 1
	 ->	T = [L|R],
		N2 is N // 2,
		(K < N2
		 ->	bintree0(K,L,E,N2)
		 ;	K1 is K - N2,
			bintree0(K1,R,E,N2)
		)
	 ;      K =:= 0,
		T = E
	).


% K is unbound
log_ith1(K,[L|_R],E,N) :-
	bintree1(K,L,E,N).
log_ith1(K,[_L|R],E,N) :-
	N1 is N + N,
	log_ith1(K1,R,E,N1),
	K is K1 + N.

% First arg (K) is unbound
bintree1(0,E,E,1).
bintree1(K,[L|R],E,N) :-
	N > 1,
	N2 is N // 2,
	(bintree1(K,L,E,N2)
	 ;
	 bintree1(K1,R,E,N2),
	 K is K1 + N2
	).

% log_ith_bound(Index,ListStr,Element) is like log_ith, but only
% succeeds if the Index_th element of ListStr is nonvariable and equal
% to Element.  This can be used in both directions, and is most useful
% with Index unbound, since it will then bind Index and Element for each
% nonvariable element in ListStr (in time proportional to N*logN, for N
% the number of nonvariable entries in ListStr.)

log_ith_bound(K,T,E) :-
	nonvar(T),
	(integer(K)	% integer
	 ->	log_ith2(K,T,E,1)
	 ;	log_ith3(K,T,E,1)
	).

log_ith2(K,[L|R],E,N) :-
	(K < N
	 ->	nonvar(L),bintree2(K,L,E,N)
	 ;	nonvar(R),
		K1 is K-N,
		N2 is N+N,
		log_ith2(K1,R,E,N2)
	).

bintree2(0,E,E,1) :- !.
bintree2(K,[L|R],E,N) :-
	N > 1,
	N2 is N // 2,
	(K < N2
	 ->	nonvar(L),
		bintree2(K,L,E,N2)
	 ;	nonvar(R),
		K1 is K - N2,
		bintree2(K1,R,E,N2)
	).

log_ith3(K,[L|_R],E,N) :-
	nonvar(L),
	bintree3(K,L,E,N).
log_ith3(K,[_L|R],E,N) :-
	nonvar(R),
	N1 is N + N,
	log_ith3(K1,R,E,N1),
	K is K1 + N.

bintree3(0,E,E,1).
bintree3(K,[L|R],E,N) :-
	N > 1,
	N2 is N // 2,
	(nonvar(L),
	 bintree3(K,L,E,N2)
	 ;
	 nonvar(R),
	 bintree3(K1,R,E,N2),
	 K is K1 + N2
	).

%% convert a log_ith structure to a list of nonempty elements
log_ith_to_list(T,L) :- log_ith_to_list(T,0,L,[]).

log_ith_to_list(T,K,L0,L) :-
	(var(T)
	 ->	L = L0
	 ;	T = [F|R],
		log_ith_to_list_btree(F,K,L0,L1),
		K1 is K+1,
		log_ith_to_list(R,K1,L1,L)
	).

log_ith_to_list_btree(T,K,L0,L) :-
	(var(T)
	 ->	L = L0
	 ; K =:= 0
	 ->	L0 = [T|L]
	 ;	T = [TL|TR],
		K1 is K-1,
		log_ith_to_list_btree(TL,K1,L0,L1),
		log_ith_to_list_btree(TR,K1,L1,L)
	).

/* log_ith_new(I,T,E) adds E to the "end" of the log_list and unifies
I to its index.  */
log_ith_new(I,T,E) :-
	(var(T)
	 ->	T = [E|_],
		I = 0
	 ;	log_ith_new_o(I,T,E,1,1)
	).

log_ith_new_o(I,[L|R],E,K,NI) :-
	(var(R),
	 log_ith_new_d(I,L,E,K,NIA)
	 ->	I is NI + NIA - 1
	 ;	NNI is 2*NI,
		K1 is K+1,
		log_ith_new_o(I,R,E,K1,NNI)
	).

log_ith_new_d(I,T,E,K,NIA) :-
	(K =< 1
	 ->	var(T),
		T=E,
		NIA = 0
	 ;	K1 is K-1,
		T = [L|R],
		(var(R),
		 log_ith_new_d(I,L,E,K1,NIA)
		 ->	true
		 ;	log_ith_new_d(I,R,E,K1,NNIA),
			NIA is NNIA + 2 ** (K1-1)
		)
	).


/* logk_ith(+KBase,+Index,?ListStr,?Element) is similar log_ith/3
except it uses a user specified base of KBase, which must be between 2
and 255.  log_ith uses binary trees with a list cons at each node;
logk_ith uses a term of arity KBase at each node.  KBase and Index
must be bound to integers. */
% :- mode logk_ith(+,+,?,?).
logk_ith(K,I,T,E) :-
	integer(K),
	integer(I),	% integer
	logk_ith0(K,I,T,E,K).

% I is bound
logk_ith0(K,I,[L|R],E,N) :-
	(I < N
	 ->	ktree0(K,I,L,E,N)
	 ;	I1 is I - N,
		N2 is K*N,
		logk_ith0(K,I1,R,E,N2)
	).

% First arg (I) is bound
ktree0(K,I,T,E,N) :-
	(var(T)
	 ->	functor(T,n,K)
	 ;	true
	),
	(N > K
	 ->	N2 is N // K,
		N3 is I // N2 + 1,
		I1 is I rem N2,  %  mod overflows?
		arg(N3,T,T1),
		ktree0(K,I1,T1,E,N2)
	 ;	I1 is I+1,
		arg(I1,T,E)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Commautils.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

comma_to_list((One,Two),[One|Twol]):- !,
	comma_to_list(Two,Twol).
comma_to_list(One,[One]).

% warning: may bind variables.
comma_member(A,','(A,_)).
comma_member(A,','(_,R)):-
	comma_member(A,R).
comma_member(A,A):- \+ (functor(A,',',2)).

comma_memberchk(A,','(A,_)):- !.
comma_memberchk(A,','(_,R)):-
	comma_memberchk(A,R).
comma_memberchk(A,A):- \+ (functor(A,',',_)).

abscomma_memberchk(A,A1):- A == A1,!.
abscomma_memberchk(','(A,_),A1):- A == A1,!.
abscomma_memberchk(','(_,R),A1):-
	abscomma_memberchk(R,A1).

comma_length(','(_L,R),N1):- !,
	comma_length(R,N),
	N1 is N + 1.
comma_length(true,0):- !.
comma_length(_,1).

comma_append(','(L,R),Cl,','(L,R1)):- !,
	comma_append(R,Cl,R1).
comma_append(true,Cl,Cl):- !.
comma_append(L,Cl,Out):-
	(Cl == true -> Out = L ; Out = ','(L,Cl)).
