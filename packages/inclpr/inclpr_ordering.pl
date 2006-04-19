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
:- module(inclpr_ordering,
	[
	    before/2,
	    after/2,
	    get_previous/2,
	    get_next/2,
	    order_compare/3,
	    new/1,
	    sv_create/2,
	    sv_merge/3,
	    sv_try_remove/3,
	    sv_remove/3,
	    sv_remove_double/3,
	    sv_remove_number/2
	]).
:- use_module(library(chr)).
:- use_module(inclpr_core,
	[
	    unify_trigger/1
	]).

:- chr_constraint
	% before(Var1,Var2)
	before(?any,?any),
	% after(Var1,Var2)
	after(?any,?any),
	% get_previous(Var1,Var2)
	get_previous(?any,?any),
	% get_next(Var1,Var2)
	get_next(?any,?any),
	% linked(Var1,Order1,Var2,Order2)
	linked(?any,+int,?any,+int),
	% new(Var)
	new(?any).

:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(toplevel_show_store,off).

% Module handling the ordering of variables, needed for round robin variable
% scheduling and some interval extensions.
%
% CHR predicates:
%
% new(Var): adds variable <Var> to the ordering if not already ordered.
% before(Var1,Var2): succeeds if <Var1> is before <Var2>, fails otherwise.
% after(Var1,Var2): succeeds if <Var1> is after <Var2>, fails otherwise.
% get_previous(Var1,Var2): <Var2> is the variable right before <Var1>.
% get_next(Var1,Var2): <Var2> is the variable right after <Var1>.

% order_compare(Relation,X,Y)
%
% Returns in <Relation> the order relation between variables <X> and <Y>
% according to the variable ordering that is established by this module.
% <Relation> is either <, = or > and can be used for sorting.

order_compare(R,X,Y) :-
	(   X == Y
	->  R = (=)
	;   before(X,Y)
	->  R = (<)
	;   R = (>)
	).

% Variable ordering is established by CHR rules. Each variable has a number
% describing its absolute order and the CHR constraints linked/4 create a
% double linked list of variables, accompagnied by this number for each pair of
% neighbour variables.

% Rules handling unification of 2 variables for the linked/4 constraints:
%
% Rule linked_unify_1 converts L-X-X-R into L-X-R.
% Rule linked_unify_2 converts L-X-M1-...-M2-X-R into L-M1-...-M2-X-R.

linked_unify_1 @ linked(L,OL,X,_), linked(X,_,X,_), linked(X,_,R,OR) <=>
	linked(L,OL,R,OR),
	unify_trigger(X).
linked_unify_2 @ linked(_,_,X,OX1), linked(X,OX1,_,_) \
    linked(L,OL,X,OX2), linked(X,OX2,R,OR) <=>
	OX1 > OX2 |
	linked(L,OL,R,OR),
	unify_trigger(X). 

% Rules handling unification of a variable with a number for the linked/4
% constraints:
%
% Rule linked_unify_3 converts L-N-R into L-R.
	
linked_unify_3 @ linked(L,OL,N,_), linked(N,_,R,OR) <=>
	number(N) |
	linked(L,OL,R,OR),
	unify_trigger(N).

% Rules for checking whether one variable is before another in the ordering:
%
% Rule before_1 fails for X is before X.
% Rule before_2 uses information from the linked list.

before_1 @ before(X,X) <=> fail.
before_2 @ linked(X,OX,_,_), linked(Y,OY,_,_) \ before(X,Y) <=> OX < OY.

% Rules for checking whether one variable is after another in the ordering:
%
% Rule after_1 fails for X is after X.
% Rule after_2 uses information from the linked list.

after_1 @ after(X,X) <=> fail.
after_2 @ linked(X,OX,_,_), linked(Y,OY,_,_) \ after(X,Y) <=> OX > OY.

% Rules for adding a new variable to the ordering:
%
% Rule new_1 handles adding a variable that is already ordered: it does not
%	change anything.
% Rule new_1 converts L-top into L-X-top with X the new variable.
% Rule new_2 creates a new ordering bottom-X-top for the case no ordering was
%	present.

new_1 @ linked(X,_,_,_) \ new(X) <=> true.
new_1 @ new(X), linked(L,OL,top,Max) <=> 
	NewMax is Max + 1,
	linked(L,OL,X,Max),
	linked(X,Max,top,NewMax).
new_2 @ new(X) <=> 
	linked(bottom,0,X,1),
	linked(X,1,top,2).

% Rules for getting the variable previous to a given variable in the ordering:
%
% Rule get_previous_1 uses information from the linked list.
% Rule get_previous_2 fails because no linked information is available.

get_previous_1 @ linked(P1,_,X,_) \ get_previous(X,P2) <=> P1 = P2.
get_previous_2 @ get_previous(_,_) <=> fail.

% Rules for getting the variable next to a given variable in the ordering:
%
% Rule get_next_1 uses information from the linked list.
% Rule get_next_2 fails because no linked information is available.

get_next_1 @ linked(X,_,N1,_) \ get_next(X,N2) <=> N1 = N2.
get_next_2 @ get_next(_,_) <=> fail.

% Sorted variable lists: a sorted variable list is a list of variables, sorted
% according to the variable ordering that is established by this module.

% sv_create(VariableList,SortedVariableList)
%
% Sorts the variables in the variable list <VariableList> into the sorted
% variable list <SortedVariableList>.

sv_create(Vars,Varlist) :-
	predsort(order_compare,Vars,Varlist).

% sv_try_remove(ListIn,Variable,ListOut)
%
% Removes an occurrence of <Variable> from sorted variable list <ListIn> if
% such exists and returns the result in sorted variable list <ListOut>. If
% <ListIn> does not contain <Variable> then <ListIn> = <ListOut>.

sv_try_remove(L1,Var,L2) :-
	(   sv_remove(L1,Var,L2)
	->  true
	;   L1 = L2
	).

% sv_remove(ListIn,Variable,ListOut)
%
% Removes an occurrence of <Variable> from sorted variable list <ListIn> and
% returns the resulting sorted variable list in <ListOut>. Fails if <ListIn>
% does not contain <Variable>. 

sv_remove([H|T],Var,L) :-
	(   H == Var
	->  L = T
	;   L = [H|T2],
	    sv_remove(T,Var,T2)
	).

% sv_remove_double(ListIn,Variable,ListOut)
%
% If <ListIn> contains two occurrences of variable <Variable>, the first
% occurrence is removed and the result is returned in <ListOut>. Otherwise,
% <ListIn> = <ListOut>. Such a situation may arise after unification of two
% variables of the list, where <Variable> is the result of that unification.

sv_remove_double([],_,[]).		% no occurrences
sv_remove_double([H|T],Var,L) :-
	(   H == Var
	->  (   sv_remove(T,Var,_)
	    ->  L = T			% two occurrences: first is removed
	    ;   sv_insert(T,Var,L)	% one occurrence
	    )
	;   L = [H|T2],
	    sv_remove_double(T,Var,T2)
	).

% sv_insert(ListIn,Variable,ListOut)
%
% Inserts variable <Variable> at the right place (according to the ordering)
% into the sorted variable list <ListIn> and returns the result in <ListOut>.

sv_insert([],Var,[Var]).
sv_insert([H1|T1],Var,[H2|T2]) :-
	(   before(H1,Var)
	->  H2 = H1,
	    sv_insert(T1,Var,T2)
	;   H2 = Var,
	    T2 = [H1|T1]
	).

% sv_remove_number(ListIn,ListOut)
%
% Removes a number from the sorted variable list <ListIn> and returns the
% result in sorted variable list <ListOut>. Such a number might be in the list
% because a variable in the list was unified with a number.

sv_remove_number([],[]).		% no occurrences
sv_remove_number([H|T],L) :-
	(   number(H)			% one occurrence
	->  L = T
	;   L = [H|T2],
	    sv_remove_number(T,T2)
	).
	
% sv_merge(ListIn1,ListIn2,MergedList)
%
% Merges the sorted duplicate free variable lists <ListIn1> and <ListIn2> into
% the duplicate free sorted variable list <MergedList>.

:- index(sv_merge(1,1)).

sv_merge([H1|T1],[H2|T2],[H3|T3]) :-
	(   before(H1,H2)
	->  H3 = H1,
	    sv_merge(T1,[H2|T2],T3)
	;   H1 == H2
	->  H3 = H1,
	    sv_merge(T1,T2,T3)
	;   H3 = H2,
	    sv_merge([H1|T1],T2,T3)
	).
sv_merge([H1|T1],[],[H1|T1]).
sv_merge([],[H2|T2],[H2|T2]).
sv_merge([],[],[]).