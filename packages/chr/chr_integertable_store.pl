/*  $Id$

    Part of CHR (Constraint Handling Rules)

    based on chr_hashtable_store (by Tom Schrijvers)
    Author:        Jon Sneyers
    E-mail:        Jon.Sneyers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, K.U. Leuven

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

% is it safe to use nb_setarg here?

:- module(chr_integertable_store,
	[ new_iht/1,
	  lookup_iht/3,
	  insert_iht/3,
	  delete_iht/3,
	  value_iht/2
	]).
:- use_module(library(lists)).
:- use_module(hprolog).

%initial_capacity(65536).
%initial_capacity(1024).
initial_capacity(8).
%initial_capacity(2).
%initial_capacity(1).


new_iht(HT) :-
	initial_capacity(Capacity),
	new_iht(Capacity,HT).

new_iht(Capacity,HT) :-
        functor(T1,t,Capacity),
        HT = ht(Capacity,Table),
        Table = T1.

lookup_iht(ht(_,Table),Int,Values) :-
	Index is Int + 1,
	arg(Index,Table,Values),
        Values \= [].
%	nonvar(Values).

insert_iht(HT,Int,Value) :-
	Index is Int + 1,
	arg(2,HT,Table),
	(arg(Index,Table,Bucket) ->
	    ( var(Bucket) ->
	    	Bucket = [Value]
	    ;
		setarg(Index,Table,[Value|Bucket])
	    )
	;	% index > capacity
		Capacity is 1<<ceil(log(Index)/log(2)),
		expand_iht(HT,Capacity),
		insert_iht(HT,Int,Value)
	).

delete_iht(ht(_,Table),Int,Value) :-
%	arg(2,HT,Table),
	Index is Int + 1,
	arg(Index,Table,Bucket),
	( Bucket = [_Value] ->
		setarg(Index,Table,[])
	;
		delete_first_fail(Bucket,Value,NBucket),
		setarg(Index,Table,NBucket)
        ).
%delete_first_fail([], Y, []).
%delete_first_fail([_], _, []) :- !.
delete_first_fail([X | Xs], Y, Xs) :-
	X == Y, !.
delete_first_fail([X | Xs], Y, [X | Zs]) :-
	delete_first_fail(Xs, Y, Zs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
value_iht(HT,Value) :-
	HT = ht(Capacity,Table),
	value_iht(1,Capacity,Table,Value).

value_iht(I,N,Table,Value) :-
	I =< N,
	arg(I,Table,Bucket),
	(
		nonvar(Bucket),
		member(Value,Bucket)
	;
		J is I + 1,
		value_iht(J,N,Table,Value)
	).
		 	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

expand_iht(HT,NewCapacity) :-
	HT = ht(Capacity,Table),
	functor(NewTable,t,NewCapacity),
	setarg(1,HT,NewCapacity),
	setarg(2,HT,NewTable),
	expand_copy(Table,1,Capacity,NewTable,NewCapacity).

expand_copy(Table,I,N,NewTable,NewCapacity) :-
	( I > N ->
		true
	;
		arg(I,Table,Bucket),
		( var(Bucket) ->
			true
		; 
			arg(I,NewTable,Bucket)
		),
		J is I + 1,
		expand_copy(Table,J,N,NewTable,NewCapacity)
	).
