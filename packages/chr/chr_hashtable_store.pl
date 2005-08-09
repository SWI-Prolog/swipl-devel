% author: Tom Schrijvers
% email:  Tom.Schrijvers@cs.kuleuven.ac.be
% copyright: K.U.Leuven, 2004

:- module(chr_hashtable_store,
	[ new_ht/1,
	  lookup_ht/3,
	  insert_ht/3,
	  delete_ht/3,
	  value_ht/2
	]).

:- use_module(pairlist).
:- use_module(hprolog).
:- use_module(library(lists)).

initial_capacity(1).

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
	    lookup_eq(Bucket,Key,Values)
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
		Inc = yes,
		LookupBucket = Key - [Value]
	; LookupBucket = K-Values ->
	      	( K == Key ->	
			( hprolog:memberchk_eq(Value,Values) ->
				true
			;
				Inc = yes,
				setarg(2,LookupBucket,[Value|Values])
			)
		;
			Inc = yes,
			setarg(LookupIndex,Table0,[Key-[Value],LookupBucket])
		)	
	;
	      	( lookup_pair_eq(LookupBucket,Key,Pair) ->
			Pair = _-Values,
			( hprolog:memberchk_eq(Value,Values) ->
				true
			;	
				Inc = yes,
				setarg(2,Pair,[Value|Values])
			)
		;
			Inc = yes,
			setarg(LookupIndex,Table0,[Key-[Value]|LookupBucket])
		)
	),
	( Inc == yes ->
		NLoad is Load + 1,
		setarg(2,HT,NLoad),
		( Load == Capacity0 ->
			expand_ht(HT,_Capacity)
		;
			true
		)
	;
		true
	).

delete_ht(HT,Key,Value) :-
	HT = ht(Capacity,Load,Table),
	NLoad is Load - 1,
	term_hash(Key,Hash),
	Index is (Hash mod Capacity) + 1,
	arg(Index,Table,Bucket),
	( var(Bucket) ->
		true
	;
		( Bucket = K-Vs ->
			( K == Key,
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
					setarg(Index,Table,NBucket)
				;
					setarg(2,Pair,NVs)
				)
			;
				true
			)
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

values_ht(HT,Values) :-
	HT = ht(Capacity,_,Table),
	values_ht(1,Capacity,Table,Values).
values_ht(I,N,Table,Values) :-
	( I =< N ->
		arg(I,Table,Bucket),
		( nonvar(Bucket) ->
			( Bucket = _-Vs ->
				append(Vs,Tail,Values)
			;
				append_snd(Bucket,Tail,Values)
			)
		;
			Values = Tail
		),
		J is I + 1,
		values_ht(J,N,Table,Tail)
	;
		Values = []
	).

append_snd([],L,L).
append_snd([_-H|Ps],L,NL) :-
	append(H,T,NL),
	append_snd(Ps,L,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_ht(HT,NewCapacity) :-
	HT = ht(Capacity,_,Table),
	NewCapacity is Capacity * 2,
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
term_hash(Term,Hash) :-
	hash_term(Term,Hash).
	
