/*  
    
    Part of INCLP(R)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, K.U. Leuven

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

:- module(benchmarks,
	[
	    broyden_banded/1,
	    feigenbaum/1,
	    moore_jones/1,
	    more_cosnard/1
	]).

:- use_module(library(inclpr),
	[
	    {}/1,
	    change_incremental/1,
	    change_standard_domain/1,
	    get_domain/2,
	    solve/0
	]).

broyden_banded(N) :-
	change_standard_domain(i(-1.0e08,1.0e08)),
	change_incremental(false),
	broyden_banded(N,Call,Names,Vars),
	time(Call),
	write_domains(Names,Vars).

feigenbaum(N) :- 
	change_standard_domain(i(0.0,1.0e02)),
	change_incremental(false),
	feigenbaum(N,Call,Names,Vars),
	time((Call,forall(solve,write_domains(Names,Vars)))).

more_cosnard(N) :-
	change_standard_domain(i(-1.0,0.0)),
	change_incremental(false),
	more_cosnard(N,Call,Names,Vars),
	time(Call),
	write_domains(Names,Vars).

moore_jones(N) :-
	change_standard_domain(i(0.0,1.0)),
	change_incremental(false),
	moore_jones(N,Call,Names,Vars),
	time(Call),
	write_domains(Names,Vars).

% Broyden-Banded Example Internal Predicates

broyden_banded(N,{Conj},Names,Vars) :-
	length(Vars,N),
	create_names(N,Names),
	numlist(1,N,Nums),
	maplist(br(N,Vars),Nums,Constraints),
	list_to_conj(Constraints,Conj).
	
br(N,Vars,I,(Xi*(2+5*Xi**2)+1-Sum)=0) :-
	nth1(I,Vars,Xi),
	br_indices(I,N,Indices),
	maplist(br_summand(Vars),Indices,Summands),
	list_to_sum(Summands,Sum).

br_summand(Vars,I,Xj*(1+Xj)) :-
	nth1(I,Vars,Xj).

br_indices(I,N,Indices) :-
	LL is max(1,I-5),
	LU is I-1,
	(   LL =< LU
	->  numlist(LL,LU,L)
	;   L = []
	),
	RL is I+1,
	RU is min(N,I+1),
	(   RL =< RU
	->  numlist(RL,RU,R)
	;   R = []
	),
	append(L,R,Indices).

% Feigenbaum Example Internal Predicates

feigenbaum(N,{Conj},Names,Vars) :-
	length(Vars,N),
	create_names(N,Names),
	Vars = [H|_],
	fb(Vars,H,Constraints),
	list_to_conj(Constraints,Conj).


fb([X],E,[-3.84*X^2+3.84*X-E=0]).
fb([X,Y|T],E,[-3.84*X^2+3.84*X-Y=0|CT]) :-
	fb([Y|T],E,CT).

% More'-Cosnard Internal Predicates

more_cosnard(N,{Conj},Names,Vars) :-
	length(Vars,N),
	create_names(N,Names),
	numlist(1,N,Nums),
	maplist(mc(Vars,Nums,N),Vars,Nums,Constraints),
	list_to_conj(Constraints,Conj).

mc(Vars,Nums,N,Var,Num,Function = 0) :-
	length(LeftVars,Num),
	length(LeftNums,Num),
	append(LeftVars,RightVars,Vars),
	append(LeftNums,RightNums,Nums),
	M is N+1,
	maplist(mc_left(M),LeftVars,LeftNums,LeftTerms),
	maplist(mc_right(M),RightVars,RightNums,RightTerms),
	list_to_sum(LeftTerms,LeftSum),
	list_to_sum(RightTerms,RightSum),
	T is Num/M,
	TM is 1-T,
	A is 1/(2*M),
	Function = Var + A*(TM*LeftSum+T*RightSum).

mc_left(M,Var,Num,Term) :-
	T is Num/M,
	TP is T+1,
	Term = T*(Var+TP)^3.

mc_right(M,Var,Num,Term) :-
	T is Num/M,
	TM is 1-T,
	TP is T+1,
	Term = TM*(Var+TP)^2.

% Moore-Jones Internal Predicates

moore_jones(N,{Conj},Names,Vars) :-
	length(Vars,N),
	create_names(N,Names),
	numlist(1,N,Nums),
	maplist(mj(Vars,N),Nums,Constraints),
	list_to_conj(Constraints,Conj).
	
mj(Vars,N,Num,Function = 0) :-
	repeat,
	(   C1 is random(N) + 1,
	    C1 =\= Num,
	    C2 is random(N) + 1,
	    C2 =\= Num,
	    C2 =\= C1,
	    C3 is random(N) + 1,
	    C3 =\= Num,
	    C3 =\= C1,
	    C3 =\= C2
	),
	!,
	B is 0.25*random(1000000000)/1000000000,
	A is (1-B)*random(1000000000)/1000000000,
	nth1(Num,Vars,Xi),
	nth1(C1,Vars,Xi1),
	nth1(C2,Vars,Xi2),
	nth1(C3,Vars,Xi3),
	Function = Xi - A - B*Xi1*Xi2*Xi3.

% Auxiliary Predicates

create_names(N,Names) :-
	length(Names,N),
	nb_digits(N,Max),
	create_names(Names,1,Max).
create_names([],_,_).
create_names([H|T],N,Max) :-
	atom_concat('X',N,Temp),
	M is N + 1,
	nb_digits(N,Digits),
	Diff is Max - Digits,
	add_spaces(Diff,Temp,H),
	create_names(T,M,Max).

nb_digits(X,N) :- N is floor(log(X)/log(10))+1.

add_spaces(0,Atom,Atom) :- !.
add_spaces(N,Temp,Atom) :-
	N > 0,	
	atom_concat(' ',Temp,New),
	M is N - 1,
	add_spaces(M,New,Atom).
	
write_domains(Names,Vars) :-
	maplist(write_domain,Names,Vars),
	nl.

write_domain(Name,Var) :- 
	get_domain(Var,i(L,U)),
	writef('%12R =< %w =< %12R\n',[L,Name,U]).

list_to_sum([],0).
list_to_sum([H|T],Sum) :-
	List = [H|T],
	length(List,N),
	list_to_sum(N,List,[],Sum).

list_to_sum(1,[H|T],T,H) :- !.
list_to_sum(2,[H1,H2|T],T,H1+H2) :- !.
list_to_sum(N,List,Tail,LeftSum+RightSum) :-
	LeftN is N >> 1,
	RightN is N - LeftN,
	list_to_sum(LeftN,List,Temp,LeftSum),
	list_to_sum(RightN,Temp,Tail,RightSum).

list_to_conj([],true).
list_to_conj([H|T],Conj) :- list_to_conj(T,H,Conj).
list_to_conj([],Element,Element).
list_to_conj([H|T],Element,(Element,Conj)) :-
	list_to_conj(T,H,Conj).