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

:- module(inclpr_inversion,
	[
	    all_occurrences/1,
	    invert/3
	]).
:- use_module(library(lists),
	[
	    reverse/2
	]).

% Module for creating the inverse of functions with one occurrence per variable
% and for counting the number of variable occurrences.

% invert(Function,Variable,Inverse)
%
% Creates the inverse of function <Function> with respect to variable
% <Variable> and stores it into <Inverse>. This means that a constraint
% <Function> = 0 is transformed into an equivalent constraint
% <Variable> = <Inverse>.

invert(Term,Var,Inverse) :-
	get_attr(Var,inclpr_occurrence_count,one(ReversePath)),
	reverse(ReversePath,Path),
	invert(Term,Path,i(-0.0,0.0),Inverse).

% invert(Function,Path,InverseIn,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> into the equivalent
% constraint <Variable> = <InverseOut> where <Variable> is located by following
% the path given by <Path>.
% A path is a list of 1's and 2's and represents where in the tree formed by
% binary operators the variable is located. For example if <Path> is the list
% [1,2] and <Function> is (A+B)*(C+D) then first the subfunction A+B is chosen
% because the first element of path is 1 and then B is chosen because the
% second element of path is 2. Paths are built while counting the number of
% occurrences for each variable.

invert(Term,Path,InverseIn,InverseOut) :-
	(   var(Term)
	->  InverseOut = InverseIn
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  Path = [H|T],
		arg(1,Term,L),
		arg(2,Term,R),
		(   H = 1
		->  invert_binary_left(Op,InverseIn,R,InverseTemp),
		    invert(L,T,InverseTemp,InverseOut)
		;   invert_binary_right(Op,L,InverseIn,InverseTemp),
		    invert(R,T,InverseTemp,InverseOut)
		)
	    ;	arg(1,Term,X),
		invert(X,Path,-InverseIn,InverseOut)
	    )
	).

% invert_binary_left(Operator,InverseIn,Right,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> where <Function> is composed
% of binary operator <Operator> applied to arguments <Left> and <Right> into an
% equivalent constraint <Left> = <InverseOut>.

invert_binary_left(+,L,R,L-R).
invert_binary_left(-,L,R,L+R).
invert_binary_left(*,L,R,L/R).
invert_binary_left(/,L,R,L*R).
invert_binary_left(**,L,R,root(L,R)).

% invert_binary_right(Operator,Left,InverseIn,InverseOut)
%
% Converts the constraint <Function> = <InverseIn> where <Function> is composed
% of binary operator <Operator> applied to arguments <Left> and <Right> into an
% equivalent constraint <Right> = <InverseOut>.

invert_binary_right(+,L,R,R-L).
invert_binary_right(-,L,R,L-R).
invert_binary_right(*,L,R,R/L).
invert_binary_right(/,L,R,L/R).

% all_occurrences(Function)
%
% Counts all occurrences of all variables in <Function> and stores them in
% the attribute inclpr_occurrence_count of each variable. The attribute either
% is the term `one(ReversePath)' with <ReversePath> the reverse of a path (see
% invert/4) or the term `more' for variables occurring more than once.
% Expects that all functors are either unary or binary

all_occurrences(Term) :- all_occurrences(Term,[]).

% all_occurrences(Function,ReversePath)
%
% The same as all_occurrences/1 but with a given initial reverse path in
% <ReversePath>.
		
all_occurrences(Term,ReversePath) :-
	(   var(Term)
	->  (   get_attr(Term,inclpr_occurrence_count,_)
	    ->  put_attr(Term,inclpr_occurrence_count,more)
	    ;   put_attr(Term,inclpr_occurrence_count,one(ReversePath))
	    )
	;   number(Term)
	->  true
	;   functor(Term,_,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,Arg1),
		arg(2,Term,Arg2),
		all_occurrences(Arg1,[1|ReversePath]),
		all_occurrences(Arg2,[2|ReversePath])
	    ;	arg(1,Term,Arg),
		all_occurrences(Arg,ReversePath)
	    )
	).