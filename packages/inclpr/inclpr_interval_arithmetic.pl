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

:- module(inclpr_interval_arithmetic,
	[
	    ia_binary_operator/4,
	    ia_unary_operator/3,
	    ia_eval/2,
	    ia_eval_g/2,
	    interval/1,
	    interval_additive_inverse/2,
	    interval_center/2,
	    interval_new_class/2,
	    interval_contains/2,
	    interval_plus/3,
	    interval_minus/3,
	    interval_div/3,
	    interval_mult/3,
	    interval_power/3,
	    interval_intersection/3,
	    interval_union/3,
	    interval_exclude_zero_bound/2,
	    interval_split_excluding_zero/3,
	    interval_negative_part/2,
	    interval_positive_part/2,
	    subdivide/3,
	    eq_subdivide/3,
	    uneq_subdivide/4,
	    accepted_solution_domain/1,
	    accepted_solution_domain_2/1,
	    minimal_interval/3
	]).

% Module implementing the Prolog part of interval arithmetic.

:- load_foreign_library(foreign(inclpr_interval_arithmetic)).

% interval(I)
%
% Succeeds if <I> is an interval.

interval(I) :- 
	nonvar(I),
	functor(I,i,2).

% interval(I,L,U)
%
% Succeeds if <I> is the interval with lower bound <L> and upper bound <U>.

interval(I,L,U) :-
	nonvar(I),
	I = i(L,U).

% ia_eval(Expression,Result)
%
% Evaluates the expression <Expression> in interval arithmetic. Nonground
% subterms are kept as they are. The result is returned in <Result>.

ia_eval(Exp,Res) :-
	ia_eval(Exp,_,Res).

% ia_eval(Expression,Ground,Result)
%
% Similar to ia_eval/2, but returns in <Ground> whether or not <Expression> is
% ground. If so, <Ground> = 1, otherwise, <Ground> = 0.

ia_eval(Exp,G,Res) :-
	(   nonvar(Exp),
	    functor(Exp,F,A)
	->  (   A =:= 2
	    ->  (   F = i
		->  Res = Exp,
		    G = 1
		;   arg(1,Exp,L),
		    arg(2,Exp,R),
		    ia_eval(L,LG,LRes),
		    ia_eval(R,RG,RRes),
		    G is LG /\ RG,
		    (   G =:= 1
		    ->  ia_binary_operator(F,LRes,RRes,Res)
		    ;   functor(Res,F,2),
			arg(1,Res,LRes),
			arg(2,Res,RRes)
		    )
		)
	    ;   A =:= 1
	    ->  arg(1,Exp,X),
		ia_eval(X,G,XRes),
		(   G =:= 1
		->  ia_unary_operator(F,XRes,Res)
		;   functor(Res,F,1),
		    arg(1,Res,XRes)
		)
	    ;   A =:= 3
	    ->  arg(1,Exp,Arg1),
		arg(2,Exp,Arg2),
		arg(3,Exp,Arg3),
		ia_eval(Arg1,Arg1G,Arg1Res),
		ia_eval(Arg2,Arg2G,Arg2Res),
		ia_eval(Arg3,Arg3G,Arg3Res),
		G is Arg1G /\ Arg2G /\ Arg3G,
		(   G =:= 1
		->  ia_ternary_operator(F,Arg1Res,Arg2Res,Arg3Res,Res)
		;   functor(Res,F,3),
		    arg(1,Res,Arg1Res),
		    arg(2,Res,Arg2Res),
		    arg(3,Res,Arg3Res)
		)
	    )	
	;   Res = Exp,
	    G = 0
	).

% ia_eval_g(Expression,Interval)
%
% Evaluates ground expression <Expression> in interval arithmetic and returns
% the result in <Interval>.

ia_eval_g(In,i(LOut,ROut)) :-
    ia_eval_g_2(In,LOut,ROut).

% ia_eval_g_2(Expression,Lower,Upper)
%
% Similar to ia_eval_g/2, but returns the result as a lower and upper bound
% instead of an interval. Somewhat faster.

ia_eval_g_2(i(L,R),L,R).
ia_eval_g_2(A+B,CL,CR) :-
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_sum_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(A-B,CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_difference_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(-A,BL,BR) :-
	ia_eval_g_2(A,AL,AR),
	ia_additive_inverse_2(AL,AR,BL,BR).
ia_eval_g_2(A*B,CL,CR) :-
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_product_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(A/B,CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_quotient_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(A**B,CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	BL =:= BR,
	ia_power_2(AL,AR,BR,CL,CR).
ia_eval_g_2(A\/B,CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_union_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(A/\B,CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_intersection_2(AL,AR,BL,BR,CL,CR).
ia_eval_g_2(root(A,B),CL,CR) :- 
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	BL =:= BR,
	ia_root_2(AL,AR,BR,CL,CR).
ia_eval_g_2(spower(A,B,C),DL,DR) :-
	ia_eval_g_2(A,AL,AR),
	ia_eval_g_2(B,BL,BR),
	ia_eval_g_2(C,CL,CR),
	CL =:= CR,
	ia_slope_power_2(AL,AR,BL,BR,CR,DL,DR).

% interval_plus(X,Y,Z)
%
% Interval <Z> is the interval sum of <X> and <Y>.

interval_plus(X,Y,Z) :- ia_sum(X,Y,Z).

% interval_minus(X,Y,Z)
%
% Interval <Z> is the interval difference of <X> and <Y>.

interval_minus(X,Y,Z) :- ia_difference(X,Y,Z).

% interval_mult(X,Y,Z)
%
% Interval <Z> is the interval product of <X> and <Y>.

interval_mult(X,Y,Z) :- ia_product(X,Y,Z).

% interval_div(X,Y,Z)
%
% Interval <Z> is the interval quotient of <X> and <Y>.

interval_div(X,Y,Z) :- ia_quotient(X,Y,Z).

% interval_additive_inverse(X,Y)
%
% Interval <Y> is the interval additive inverse of <X>.

interval_additive_inverse(X,Y) :- ia_additive_inverse(X,Y).

% interval_split_excluding_zero(X,Y,Z)
%
% Interval <X> is split into the strictly negative interval <Y> and the
% strictly positive interval <Z>.

interval_split_excluding_zero(X,Y,Z) :- ia_split_excluding_zero(X,Y,Z).

% interval_exclude_zero_bound(X,Y)
%
% If interval <X> is i(-0.0,U) then interval <Y> is i(0.0,U).
% If interval <X> is i(L,0.0) then interval <Y> is i(L,-0.0).

interval_exclude_zero_bound(X,Y) :- ia_exclude_zero_bound(X,Y).

% interval_power(X,Y,Z)
%
% Interval <Z> is the interval power of <X> and <Y>.

interval_power(X,Y,Z) :- ia_power(X,Y,Z).

% interval_new_class(X,C)
%
% <C> is the class of interval <X>:
% - `n' if <X> is strictly negative
% - `z' if <X> contains zero
% - `p' if <X> is strictly positive

interval_new_class(X,C) :- ia_class(X,C).

% ia_binary_operator(Operator,Left,Right,Result)
%
% Interval <Result> is the result of applying the binary operator <Operator>
% in interval arithmetic to <Left> and <Right> if both <Left> and <Right> are
% intervals. Otherwise <Result> = <Operator>(<Left>,<Right>).

ia_binary_operator(Op,L,R,Res) :- 
	(   interval(L),
	    interval(R)
	->  ia_binary_operator_g(Op,L,R,Res)
	;   functor(Res,Op,2),
	    arg(1,Res,L),
	    arg(2,Res,R)
	).

% ia_unary_operator(Operator,Argument,Result)
%
% Interval <Result> is the result of applying the unary operator <Operator>
% in interval arithmetic to <Argument> if <Argument> is an interval. Otherwise
% <Result> = <Operator>(<Argument>).

ia_unary_operator(Op,X,Res) :-
	(   interval(X)
	->  ia_unary_operator_g(Op,X,Res)
	;   functor(Res,Op,1),
	    arg(1,Res,X)
	).

% ia_ternary_operator(Operator,Left,Middle,Right,Result)
%
% Interval <Result> is the result of applying the ternary operator <Operator>
% in interval arithmetic to <Left>, <Middle> and <Right> if all three are
% intervals. Otherwise <Result> = <Operator>(<Left>,<Middle>,<Right>).

ia_ternary_operator(Op,A,B,C,Res) :-
	(   interval(A),
	    interval(B),
	    interval(C)
	->  ia_ternary_operator_g(Op,A,B,C,Res)
	;   functor(Res,Op,3),
	    arg(1,Res,A),
	    arg(2,Res,B),
	    arg(3,Res,C)
	).

% ia_binary_operator_g(Operator,Left,Right,Result)
%
% Interval <Result> is the result of applying the binary operator <Operator>
% in interval arithmetic to intervals <Left> and <Right>.

:- hash(ia_binary_operator/4).

ia_binary_operator_g((+) ,L,R,Res) :- ia_sum(L,R,Res).
ia_binary_operator_g((-) ,L,R,Res) :- ia_difference(L,R,Res).
ia_binary_operator_g((*) ,L,R,Res) :- ia_product(L,R,Res).
ia_binary_operator_g((**),L,R,Res) :- ia_power(L,R,Res).
ia_binary_operator_g((/) ,L,R,Res) :- ia_quotient(L,R,Res).
ia_binary_operator_g((\/),L,R,Res) :- ia_union(L,R,Res).
ia_binary_operator_g((/\),L,R,Res) :- ia_intersection(L,R,Res).
ia_binary_operator_g(root,L,R,Res) :- ia_root(L,R,Res).

% ia_ternary_operator_g(Operator,Left,Middle,Right,Result)
%
% Interval <Result> is the result of applying the ternary operator <Operator>
% in interval arithmetic to intervals <Left>, <Middle> and <Right>.

ia_ternary_operator_g(spower,i(AL,AR),i(BL,BR),i(CL,CR),i(DL,DR)) :-
	CL =:= CR,
	ia_slope_power_2(AL,AR,BL,BR,CR,DL,DR). 

% ia_unary_operator_g(Operator,Argument,Result)
%
% Interval <Result> is the result of applying the unary operator <Operator>
% in interval arithmetic to interval <Argument>.

ia_unary_operator_g((-),X,Res) :- ia_additive_inverse(X,Res).

% interval_intersection(Left,Right,Intersection)
%
% Intersects the interval or union of intervals in <Left> by the interval
% or union of intervals in <Right> and returns the resulting interval or
% union of intervals in <Intersection>. The union operator is represented as a
% term u(X,Y) with <X> and <Y> intervals or union of intervals.

:- index(interval_intersection(1,1,0)).
interval_intersection(i(A,B),i(C,D),I) :- 
	ia_intersection(i(A,B),i(C,D),I).
interval_intersection(u(A,B),i(C,D),I) :-
	(   interval_intersection(A,i(C,D),AI)
	->  (   interval_intersection(B,i(C,D),BI)
	    ->  I = u(AI,BI)
	    ;   I = AI
	    )
	;   interval_intersection(B,i(C,D),I)
	).
interval_intersection(u(A,B),u(C,D),I) :-
	(   interval_intersection(u(A,B),C,CI)
	->  (   interval_intersection(u(A,B),D,DI)
	    ->  I = u(CI,DI)
	    ;   I = CI
	    )
	;   interval_intersection(u(A,B),D,I)
	).

% interval_union(Left,Right,Union)
%
% Returns in <Union> the smallest interval containing all intervals in <Left>
% and <Right>. <Left> and <Right> are either intervals or a union of intervals.

:- index(interval_union(1,1,0)).
interval_union(i(A,B),i(C,D),U) :- 
	ia_union(i(A,B),i(C,D),U).
interval_union(u(A,B),i(C,D),U) :-
	interval_union(A,i(C,D),AU),
	interval_union(B,i(C,D),BU),
	ia_union(AU,BU,U).
interval_union(u(A,B),u(C,D),U) :-
	interval_union(u(A,B),C,CU),
	interval_union(u(A,B),D,DU),
	ia_union(CU,DU,U).

% interval_center(Interval,Center)
%
% Returns in <Center> a canonical interval containing the center of the
% interval <Interval>.

interval_center(I1,I2) :- ia_center(I1,I2).

% interval_contains(Interval,Value)
%
% Succeeds if value <Value> belongs to interval <Interval>.

interval_contains(X,V) :- ia_contains(X,V).

% interval(Numerical,Interval)
%
% If <Numerical> is an interval, <Interval> = <Numerical>. If <Numerical> is a
% number, <Interval> is a canonical interval containing <Numerical>. Fails for
% all other cases.

interval(i(L,R),i(L,R)) :- !.
interval(V,i(V,V)) :- number(V).

% subdivide(Direction,In,Out)
%
% If <Direction> is `lower', interval <Out> is the lower half of interval <In>.
% If <Direction> is `upper', interval <Out> is the upper half of interval <In>.
% Fails for all other cases.

subdivide(lower,X,Y) :- ia_split(X,0.5,Y,_).
subdivide(upper,X,Y) :- ia_split(X,0.5,_,Y).

% eq_subdivide(Interval,Lower,Upper)
%
% Subdivides interval <Interval> into two equal halves. The lower half is
% returned in <Lower>, the upper half in <Upper>.

eq_subdivide(X,Y,Z) :- ia_split(X,0.5,Y,Z).

% uneq_subdivide(Direction,Interval,Lower,Upper)
%
% Subdivides interval <Interval> into two unequal parts, one part being 30% of
% <Interval> and the other being 70% of <Interval>. The direction <Direction>
% specifies the smaller part and is either `lower' or `upper'. This is used by
% the domain narrowing operator to avoid splitting around a zero.

uneq_subdivide(lower,X,Y,Z) :- ia_split(X,0.3,Y,Z).
uneq_subdivide(upper,X,Y,Z) :- ia_split(X,0.7,Y,Z).

% minimal_interval(Direction,Interval,MinimalInterval)
%
% Returns in <MinimalInterval> an interval of small width contained in
% <Interval> where the upper bound of <MinimalInterval> is equal to the upper
% bound of <Interval> if <Direction> is `upper' and the lower bound of
% <MinimalInterval> is equal to the lower bound of <Interval> if <Direction> is
% `lower'. Used as a stopping criterion by the consistency checker/narrowing
% operator.

minimal_interval(upper,X,Int) :- neg_interval(X,Int).
minimal_interval(lower,X,Int) :- pos_interval(X,Int).

% pos_interval(Interval,MinimalInterval)
%
% Returns in <MinimalInterval> an interval of small width contained in
% <Interval> where the lower bound of <MinimalInterval> is equal to the lower
% bound of <Interval>.

pos_interval(i(L,_),i(L,U)) :- 
	P is abs(L),
	(
	  P > 1e-310 ->

	    U is L+(P*1e-11)
	;
	    U is L+1e-310
	).

% neg_interval(Interval,MinimalInterval)
%
% Returns in <MinimalInterval> an interval of small width contained in
% <Interval> where the upper bound of <MinimalInterval> is equal to the upper
% bound of <Interval>.

neg_interval(i(_,U),i(L,U)) :-
	N is abs(U),
	(
	  N > 1e-310 ->

	    L is U-(N*1e-11)
	;
	    L is U-1e-310
	). 

% accepted_solution_domain(Interval)
%
% Succeeds if the interval <Interval> is of sufficiently small width to be
% accepted as a point. This means that its relative width is less than 1e-11
% or it lies in the interval [-1e-310,1e310].

accepted_solution_domain(i(L,U)) :-
	M is max(abs(L),abs(U)),
	(
	  M > 1e-310 ->

	    (U-L) < M*1e-011
	;
	    true
	).

% accepted_solution_domain_2(Interval)
%
% Slightly more tolerant version of accepted_solution_domain/1.
% Succeeds if the interval <Interval> is of sufficiently small width to be
% accepted as a point. This means that its relative width is less than 1e-10
% or it lies in the interval [-1e-310,1e310].

accepted_solution_domain_2(i(L,U)) :-
	M is max(abs(L),abs(U)),
	(
	  M > 1e-310 ->

	    (U-L) < M*1e-010
	;
	    true
	).

% interval_negative_part(Interval,NegativePart)
%
% Returns in <NegativePart> the negative part of interval <Interval> if such
% exists. Fails otherwise.

interval_negative_part(X,Y) :- ia_negative_part(X,Y).

% interval_positive_part(Interval,PositivePart)
%
% Returns in <PositivePart> the positive part of interval <Interval> if such
% exists. Fails otherwise.

interval_positive_part(X,Y) :- ia_positive_part(X,Y).