/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.ac.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005-2006, K.U. Leuven

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
:- module(chr_compiler_utility,
	[ is_variant/2
	, time/2
	, replicate/3
	, pair_all_with/3
	, conj2list/2
	, list2conj/2
	, disj2list/2
	, list2disj/2
	, variable_replacement/3
	, variable_replacement/4
	, identical_rules/2
	, copy_with_variable_replacement/3
	, my_term_copy/3
	, my_term_copy/4
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
is_variant(A,B) :-
	copy_term_nat(A,AC),
	copy_term_nat(B,BC),
	term_variables(AC,AVars), 
	term_variables(BC,BVars),
	AC = BC,
	is_variant1(AVars),
	is_variant2(BVars).

is_variant1([]).
is_variant1([X|Xs]) :-
	var(X),
	X = '$test',
	is_variant1(Xs).
	
is_variant2([]).
is_variant2([X|Xs]) :-
	X == '$test',
	is_variant2(Xs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
time(Phase,Goal) :-
	statistics(runtime,[T1|_]),
	call(Goal),
	statistics(runtime,[T2|_]),
	T is T2 - T1,
	format('    ~w:\t\t~w ms\n',[Phase,T]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replicate(N,E,L) :-
	( N =< 0 ->
		L = []
	;
		L = [E|T],
		M is N - 1,
		replicate(M,E,T)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
pair_all_with([],_,[]).
pair_all_with([X|Xs],Y,[X-Y|Rest]) :-
	pair_all_with(Xs,Y,Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conj2list(Conj,L) :-				%% transform conjunctions to list
  conj2list(Conj,L,[]).

conj2list(Conj,L,T) :-
  Conj = (true,G2), !,
  conj2list(G2,L,T).
conj2list(Conj,L,T) :-
  Conj = (G1,G2), !,
  conj2list(G1,L,T1),
  conj2list(G2,T1,T).
conj2list(G,[G | T],T).

disj2list(Conj,L) :-				%% transform disjunctions to list
  disj2list(Conj,L,[]).
disj2list(Conj,L,T) :-
  Conj = (fail;G2), !,
  disj2list(G2,L,T).
disj2list(Conj,L,T) :-
  Conj = (G1;G2), !,
  disj2list(G1,L,T1),
  disj2list(G2,T1,T).
disj2list(G,[G | T],T).

list2conj([],true).
list2conj([G],X) :- !, X = G.
list2conj([G|Gs],C) :-
	( G == true ->				%% remove some redundant trues
		list2conj(Gs,C)
	;
		C = (G,R),
		list2conj(Gs,R)
	).

list2disj([],fail).
list2disj([G],X) :- !, X = G.
list2disj([G|Gs],C) :-
	( G == fail ->				%% remove some redundant fails
		list2disj(Gs,C)
	;
		C = (G;R),
		list2disj(Gs,R)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% check wether two rules are identical

identical_rules(rule(H11,H21,G1,B1),rule(H12,H22,G2,B2)) :-
   G1 == G2,
   identical_bodies(B1,B2),
   permutation(H11,P1),
   P1 == H12,
   permutation(H21,P2),
   P2 == H22.

identical_bodies(B1,B2) :-
   ( B1 = (X1 = Y1),
     B2 = (X2 = Y2) ->
     ( X1 == X2,
       Y1 == Y2
     ; X1 == Y2,
       X2 == Y1
     ),
     !
   ; B1 == B2
   ).
 
% replace variables in list
   
copy_with_variable_replacement(X,Y,L) :-
   ( var(X) ->
     ( chr_translate:lookup_eq(L,X,Y) ->
       true
     ; X = Y
     )
   ; functor(X,F,A),
     functor(Y,F,A),
     X =.. [_|XArgs],
     Y =.. [_|YArgs],
     copy_with_variable_replacement_l(XArgs,YArgs,L)
   ).

copy_with_variable_replacement_l([],[],_).
copy_with_variable_replacement_l([X|Xs],[Y|Ys],L) :-
   copy_with_variable_replacement(X,Y,L),
   copy_with_variable_replacement_l(Xs,Ys,L).
   
%% build variable replacement list

variable_replacement(X,Y,L) :-
   variable_replacement(X,Y,[],L).
   
variable_replacement(X,Y,L1,L2) :-
   ( var(X) ->
     var(Y),
     ( chr_translate:lookup_eq(L1,X,Z) ->
       Z == Y,
       L2 = L1
     ; ( X == Y -> L2=L1 ; L2 = [X-Y,Y-X|L1])
     )
   ; X =.. [F|XArgs],
     nonvar(Y),
     Y =.. [F|YArgs],
     variable_replacement_l(XArgs,YArgs,L1,L2)
   ).

variable_replacement_l([],[],L,L).
variable_replacement_l([X|Xs],[Y|Ys],L1,L3) :-
   variable_replacement(X,Y,L1,L2),
   variable_replacement_l(Xs,Ys,L2,L3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_term_copy(X,Dict,Y) :-
   my_term_copy(X,Dict,_,Y).

my_term_copy(X,Dict1,Dict2,Y) :-
   (   var(X) ->
       (   chr_translate:lookup_eq(Dict1,X,Y) ->
           Dict2 = Dict1
       ;   Dict2 = [X-Y|Dict1]
       )
   ;   functor(X,XF,XA),
       functor(Y,XF,XA),
       X =.. [_|XArgs],
       Y =.. [_|YArgs],
       my_term_copy_list(XArgs,Dict1,Dict2,YArgs)
   ).

my_term_copy_list([],Dict,Dict,[]).
my_term_copy_list([X|Xs],Dict1,Dict3,[Y|Ys]) :-
   my_term_copy(X,Dict1,Dict2,Y),
   my_term_copy_list(Xs,Dict2,Dict3,Ys).

