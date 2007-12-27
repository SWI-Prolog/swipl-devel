/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
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
% author: Tom Schrijvers
% email:  Tom.Schrijvers@cs.kuleuven.be
% copyright: K.U.Leuven, 2004

:- module(chr_hashtable_store,
	[ new_ht/1,
	  lookup_ht/3,
	  lookup_ht1/4,
	  insert_ht/3,
	  insert_ht/4,
	  delete_ht/3,
	  delete_first_ht/3,
	  value_ht/2,
	  stats_ht/1,
	  stats_ht/1
	]).

:- use_module(pairlist).
:- use_module(hprolog).
:- use_module(library(lists)).

:- multifile user:goal_expansion/2.
:- dynamic user:goal_expansion/2.

user:goal_expansion(term_hash(Term,Hash),hash_term(Term,Hash)).

% term_hash(Term,Hash) :-
% 	hash_term(Term,Hash).
initial_capacity(89).

new_ht(HT) :-
	initial_capacity(Capacity),
	new_ht(Capacity,HT).

new_ht(Capacity,HT) :-
	functor(T1,t,Capacity),
	HT = ht(Capacity,0,Table),
	Table = T1.

lookup_ht(HT,Key,Values) :-
	term_hash(Key,Hash),
	HT = ht(Capacity,_,Table),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	nonvar(Bucket),
	( Bucket = K-Vs ->
	    K == Key,	
	    Values = Vs
	;
	    lookup(Bucket,Key,Values)
	).

lookup_ht1(HT,Hash,Key,Values) :-
	HT = ht(Capacity,_,Table),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	nonvar(Bucket),
	( Bucket = K-Vs ->
	    K == Key,	
	    Values = Vs
	;
	    lookup(Bucket,Key,Values)
	).

lookup_pair_eq([P | KVs],Key,Pair) :-
	P = K-_,
	( K == Key ->
		P = Pair
	;
		lookup_pair_eq(KVs,Key,Pair)
	).

insert_ht(HT,Key,Value) :-
	term_hash(Key,Hash),
	HT = ht(Capacity0,Load,Table0),
	LookupIndex is (Hash mod Capacity0) + 1,
	arg(LookupIndex,Table0,LookupBucket),
	( var(LookupBucket) ->
		LookupBucket = Key - [Value]
	; LookupBucket = K-Values ->
		( K == Key ->	
			setarg(2,LookupBucket,[Value|Values])
		;
			setarg(LookupIndex,Table0,[Key-[Value],LookupBucket])
		)	
	;
	      	( lookup_pair_eq(LookupBucket,Key,Pair) ->
			Pair = _-Values,
			setarg(2,Pair,[Value|Values])
		;
			setarg(LookupIndex,Table0,[Key-[Value]|LookupBucket])
		)
	),
	NLoad is Load + 1,
	setarg(2,HT,NLoad),
	( Load == Capacity0 ->
		expand_ht(HT,_Capacity)
	;
		true
	).

insert_ht1(HT,Key,Hash,Value) :-
	HT = ht(Capacity0,Load,Table0),
	LookupIndex is (Hash mod Capacity0) + 1,
	arg(LookupIndex,Table0,LookupBucket),
	( var(LookupBucket) ->
		LookupBucket = Key - [Value]
	; LookupBucket = K-Values ->
		( K == Key ->	
			setarg(2,LookupBucket,[Value|Values])
		;
			setarg(LookupIndex,Table0,[Key-[Value],LookupBucket])
		)	
	;
	      	( lookup_pair_eq(LookupBucket,Key,Pair) ->
			Pair = _-Values,
			setarg(2,Pair,[Value|Values])
		;
			setarg(LookupIndex,Table0,[Key-[Value]|LookupBucket])
		)
	),
	NLoad is Load + 1,
	setarg(2,HT,NLoad),
	( Load == Capacity0 ->
		expand_ht(HT,_Capacity)
	;
		true
	).

% LDK: insert version with extra argument denoting result

insert_ht(HT,Key,Value,Result) :-
	HT = ht(Capacity,Load,Table),
	term_hash(Key,Hash),
	LookupIndex is (Hash mod Capacity) + 1,
	arg(LookupIndex,Table,LookupBucket),
	(   var(LookupBucket)
	->  Result = [Value],
	    LookupBucket = Key - Result,
	    NewLoad is Load + 1
	;   LookupBucket = K - V
	->  (   K = Key
	    ->  Result = [Value|V],
		setarg(2,LookupBucket,Result),
		NewLoad = Load
	    ;   Result = [Value],
		setarg(LookupIndex,Table,[Key - Result,LookupBucket]),
		NewLoad is Load + 1
	    )	
	;   (   lookup_pair_eq(LookupBucket,Key,Pair)
	    ->  Pair = _-Values,
		Result = [Value|Values],
		setarg(2,Pair,Result),
		NewLoad = Load
	    ;   Result = [Value],
		setarg(LookupIndex,Table,[Key - Result|LookupBucket]),
		NewLoad is Load + 1
	    )
	),
	setarg(2,HT,NewLoad),
	(   NewLoad > Capacity
	->  expand_ht(HT,_)
	;   true
	).

% LDK: deletion of the first element of a bucket
delete_first_ht(HT,Key,Values) :-
	HT = ht(Capacity,Load,Table),
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	(   Bucket = _-[_|Values]
	->  (   Values = []
	    ->  setarg(Index,Table,_),
		NewLoad is Load - 1
	    ;   setarg(2,Bucket,Values),
		NewLoad = Load
	    )
	;   lookup_pair_eq(Bucket,Key,Pair)
	->  Pair = _-[_|Values],
	    (   Values = []
	    ->  pairlist_delete_eq(Bucket,Key,NewBucket),
		(   NewBucket = []
		->  setarg(Index,Table,_)
		;   NewBucket = [OtherPair]
		->  setarg(Index,Table,OtherPair)
		;   setarg(Index,Table,NewBucket)
		),
		NewLoad is Load - 1
	    ;   setarg(2,Pair,Values),
		NewLoad = Load
	    )
	).

delete_ht(HT,Key,Value) :-
	HT = ht(Capacity,Load,Table),
	NLoad is Load - 1,
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	( /* var(Bucket) ->
		true
	; */ Bucket = K-Vs ->
		( /* K == Key, */
		  delete_first_fail(Vs,Value,NVs) ->
			setarg(2,HT,NLoad),
			( NVs == [] ->
				setarg(Index,Table,_)
			;
				setarg(2,Bucket,NVs)
			)
		;
			true
		)	
	; 
		( lookup_pair_eq(Bucket,Key,Pair),
		  Pair = _-Vs,
		  delete_first_fail(Vs,Value,NVs) ->
			setarg(2,HT,NLoad),
			( NVs == [] ->
				pairlist_delete_eq(Bucket,Key,NBucket),
				( NBucket = [Singleton] ->
					setarg(Index,Table,Singleton)
				;
					setarg(Index,Table,NBucket)
				)
			;
				setarg(2,Pair,NVs)
			)
		;
			true
		)
	).

delete_first_fail([X | Xs], Y, Zs) :-
	( X == Y ->
		Zs = Xs
	;
		Zs = [X | Zs1],
		delete_first_fail(Xs, Y, Zs1)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
value_ht(HT,Value) :-
	HT = ht(Capacity,_,Table),
	value_ht(1,Capacity,Table,Value).

value_ht(I,N,Table,Value) :-
	I =< N,
	arg(I,Table,Bucket),
	(
		nonvar(Bucket),
		( Bucket = _-Vs ->
			true
		;
			member(_-Vs,Bucket)
		),
		member(Value,Vs)
	;
		J is I + 1,
		value_ht(J,N,Table,Value)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_ht(HT,NewCapacity) :-
	HT = ht(Capacity,_,Table),
	NewCapacity is Capacity * 2 + 1,
	functor(NewTable,t,NewCapacity),
	setarg(1,HT,NewCapacity),
	setarg(3,HT,NewTable),
	expand_copy(Table,1,Capacity,NewTable,NewCapacity).

expand_copy(Table,I,N,NewTable,NewCapacity) :-
	( I > N ->
		true
	;
		arg(I,Table,Bucket),
		( var(Bucket) ->
			true
		; Bucket = Key - Value ->
			expand_insert(NewTable,NewCapacity,Key,Value)
		;
			expand_inserts(Bucket,NewTable,NewCapacity)
		),
		J is I + 1,
		expand_copy(Table,J,N,NewTable,NewCapacity)
	).

expand_inserts([],_,_).
expand_inserts([K-V|R],Table,Capacity) :-
	expand_insert(Table,Capacity,K,V),
	expand_inserts(R,Table,Capacity).

expand_insert(Table,Capacity,K,V) :-
	term_hash(K,Hash),	
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	( var(Bucket) ->
		Bucket = K - V
	; Bucket = _-_ ->
		setarg(Index,Table,[K-V,Bucket])
	;
		setarg(Index,Table,[K-V|Bucket])
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
stats_ht(HT) :-	
	HT = ht(Capacity,Load,Table),
	format('HT load = ~w / ~w\n',[Load,Capacity]),
	( between(1,Capacity,Index),
		arg(Index,Table,Entry),
		( var(Entry)  -> Size = 0
		; Entry = _-_ -> Size = 1
		; length(Entry,Size)
		),
		format('~w : ~w\n',[Index,Size]),
		fail
	;
		true
	).
