:- module(hprolog,
	  [ substitute_eq/4,		% +OldVal, +OldList, +NewVal, -NewList
	    memberchk_eq/2,		% +Val, +List
	    intersect_eq/3,		% +List1, +List2, -Intersection
	    list_difference_eq/3,	% +List, -Subtract, -Rest
	    take/3,			% +N, +List, -FirstElements
	    drop/3,			% +N, +List, -LastElements
	    split_at/4,			% +N, +List, -FirstElements, -LastElements
	    max_go_list/2,		% +List, -Max
	    or_list/2,			% +ListOfInts, -BitwiseOr
	    sublist/2,			% ?Sublist, +List
	    bounded_sublist/3,		% ?Sublist, +List, +Bound
	    chr_delete/3,
	    init_store/2,
	    get_store/2,
	    update_store/2,
	    make_get_store_goal/3,
	    make_update_store_goal/3,
	    make_init_store_goal/3,

	    empty_ds/1,
	    ds_to_list/2,
	    get_ds/3,
	    put_ds/4
	  ]).
:- use_module(library(lists)).
:- use_module(library(assoc)).

empty_ds(DS) :- empty_assoc(DS).
ds_to_list(DS,LIST) :- assoc_to_list(DS,LIST).
get_ds(A,B,C) :- get_assoc(A,B,C).
put_ds(A,B,C,D) :- put_assoc(A,B,C,D).


init_store(Name,Value) :- nb_setval(Name,Value).

get_store(Name,Value) :- nb_getval(Name,Value).

update_store(Name,Value) :- b_setval(Name,Value).

make_init_store_goal(Name,Value,Goal) :- Goal = nb_setval(Name,Value).

make_get_store_goal(Name,Value,Goal) :- Goal = nb_getval(Name,Value).

make_update_store_goal(Name,Value,Goal) :- Goal = b_setval(Name,Value).


		 /*******************************
		 *      MORE LIST OPERATIONS	*
		 *******************************/

%	substitute_eq(+OldVal, +OldList, +NewVal, -NewList)
%	
%	Substitute OldVal by NewVal in OldList and unify the result
%	with NewList.

substitute_eq(_, [], _, []) :- ! .
substitute_eq(X, [U|Us], Y, [V|Vs]) :-
        (   X == U
	->  V = Y,
            substitute_eq(X, Us, Y, Vs)
        ;   V = U,
            substitute_eq(X, Us, Y, Vs)
        ).

%	memberchk_eq(+Val, +List)
%	
%	Deterministic check of membership using == rather than
%	unification.

memberchk_eq(X, [Y|Ys]) :-
   (   X == Y
   ->  true
   ;   memberchk_eq(X, Ys)
   ).

% :- load_foreign_library(chr_support).

%	list_difference_eq(+List, -Subtract, -Rest)
%	
%	Delete all elements of Subtract from List and unify the result
%	with Rest.  Element comparision is done using ==/2.

list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
	(   memberchk_eq(X,Ys)
	->  list_difference_eq(Xs,Ys,L)
	;   L = [X|T],
	    list_difference_eq(Xs,Ys,T)
	).

%	intersect_eq(+List1, +List2, -Intersection)
%	
%	Determine the intersection of two lists without unifying values.

intersect_eq([], _, []).
intersect_eq([X|Xs], Ys, L) :-
	(   memberchk_eq(X, Ys)
	->  L = [X|T],
	    intersect_eq(Xs, Ys, T)
	;   intersect_eq(Xs, Ys, L)
	).


%	take(+N, +List, -FirstElements)
%	
%	Take the first  N  elements  from   List  and  unify  this  with
%	FirstElements. The definition is based   on the GNU-Prolog lists
%	library. Implementation by Jan Wielemaker.

take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).

%	Drop the first  N  elements  from   List  and  unify  the remainder  with
%	LastElements.

drop(0,LastElements,LastElements) :- !.
drop(N,[_|Tail],LastElements) :-
	N > 0,
	N1 is N  - 1,
	drop(N1,Tail,LastElements).

split_at(0,L,[],L) :- !.
split_at(N,[H|T],[H|L1],L2) :-
	M is N -1,
	split_at(M,T,L1,L2).

%	max_go_list(+List, -Max)
%	
%	Return the maximum of List in the standard order of terms.

max_go_list([H|T], Max) :-
	max_go_list(T, H, Max).

max_go_list([], Max, Max).
max_go_list([H|T], X, Max) :-
        (   H @=< X
	->  max_go_list(T, X, Max)
        ;   max_go_list(T, H, Max)
        ).

%	or_list(+ListOfInts, -BitwiseOr)
%	
%	Do a bitwise disjuction over all integer members of ListOfInts.

or_list(L, Or) :-
	or_list(L, 0, Or).

or_list([], Or, Or).
or_list([H|T], Or0, Or) :-
	Or1 is H \/ Or0,
	or_list(T, Or1, Or).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sublist(L, L).
sublist(Sub, [H|T]) :-
	'$sublist1'(T, H, Sub).

'$sublist1'(Sub, _, Sub).
'$sublist1'([H|T], _, Sub) :-
	'$sublist1'(T, H, Sub).
'$sublist1'([H|T], X, [X|Sub]) :-
	'$sublist1'(T, H, Sub).

bounded_sublist(Sublist,_,_) :-
	Sublist = [].
bounded_sublist(Sublist,[H|List],Bound) :-
	Bound > 0,
	(
		Sublist = [H|Rest],
		NBound is Bound - 1,
		bounded_sublist(Rest,List,NBound)
	;
		bounded_sublist(Sublist,List,Bound)
	).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

chr_delete([], _, []).
chr_delete([H|T], X, L) :-
        (   H==X ->
            chr_delete(T, X, L)
        ;   L=[H|RT],
            chr_delete(T, X, RT)
        ).
    
