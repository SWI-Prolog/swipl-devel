%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%              _      _ _     _   
%%  _ __   __ _(_)_ __| (_)___| |_ 
%% | '_ \ / _` | | '__| | / __| __|
%% | |_) | (_| | | |  | | \__ \ |_ 
%% | .__/ \__,_|_|_|  |_|_|___/\__|
%% |_|                            
%%
%% * author: Tom Schrijvers

:- module(pairlist,[
		fst_of_pairs/2,
		lookup/3,
		lookup_any/3,
		lookup_eq/3,
		lookup_any_eq/3,
		pairup/3,
		snd_of_pairs/2,
		translate/3
	]).

fst_of_pairs([],[]).
fst_of_pairs([X-_|XYs],[X|Xs]) :-
	fst_of_pairs(XYs,Xs).

snd_of_pairs([],[]).
snd_of_pairs([_-Y|XYs],[Y|Ys]) :-
	snd_of_pairs(XYs,Ys).

pairup([],[],[]).
pairup([X|Xs],[Y|Ys],[X-Y|XYs]) :-
	pairup(Xs,Ys,XYs).

lookup([K - V | KVs],Key,Value) :-
	( K = Key ->
		V = Value
	;
		lookup(KVs,Key,Value)
	).

lookup_any([K - V | KVs],Key,Value) :-
	( 
		K = Key,
		V = Value
	;
		lookup_any(KVs,Key,Value)
	).

lookup_eq([K - V | KVs],Key,Value) :-
	( K == Key ->
		V = Value
	;
		lookup_eq(KVs,Key,Value)
	).

lookup_any_eq([K - V | KVs],Key,Value) :-
	( 
		K == Key,
		V = Value
	;
		lookup_any_eq(KVs,Key,Value)
	).

translate([],_,[]).
translate([X|Xs],Dict,[Y|Ys]) :-
	lookup_eq(Dict,X,Y),
	translate(Xs,Dict,Ys).

pairlist_delete([], _, []).
pairlist_delete([K - V| KVs], Key, PL) :-
	( Key = K ->
		PL = KVs
	;
		PL = [ K - V | T ],
		pairlist_delete(KVs, Key, T)
	).

pairlist_delete_all([], _, []).
pairlist_delete_all([K - V| KVs], Key, PL) :-
	( Key = K ->
		pairlist_delete_all(KVs, Key, PL)
		
	;
		PL = [ K - V | T ],
		pairlist_delete_all(KVs, Key, T)
	).

pairlist_delete_eq([], _, []).
pairlist_delete_eq([K - V| KVs], Key, PL) :-
	( Key == K ->
		PL = KVs
	;
		PL = [ K - V | T ],
		pairlist_delete_eq(KVs, Key, T)
	).

pairlist_delete_all_eq([], _, []).
pairlist_delete_all_eq([K - V| KVs], Key, PL) :-
	( Key == K ->
		pairlist_delete_all_eq(KVs, Key, PL)
	;
		PL = [ K - V | T ],
		pairlist_delete_all_eq(KVs, Key, T)
	).
		
