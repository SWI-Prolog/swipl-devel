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
		
		
