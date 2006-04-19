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
:- module(inclpr_newton,
	[
	    interval_newton/6
	]).
:- use_module(inclpr_consistency,
	[
	    get_full_evaluation/5
	]).
:- use_module(inclpr_interval_arithmetic,
	[
	    interval_center/2,
	    interval_contains/2,
	    interval_new_class/2,
	    interval_intersection/3,
	    interval_div/3,
	    interval_exclude_zero_bound/2,
	    interval_split_excluding_zero/3,
	    interval_minus/3,
	    interval_union/3
	]).

% Module for calculating an Interval Newton domain reduction step.

% interval_newton(IntervalExtension,FunctionPartialEvaluation,
%	DerivativePartialEvaluation,Variable,Interval,Answer)
%
% Computes an Interval Newton step for the function with partially evaluated *
% interval extension <FunctionPartialEvaluation> and partially evaluated *
% interval extension of the derivative <DerivativeIntervalExtension>, with
% respect to variable <Variable> which has initial domain <Interval>. The
% result is returned in <Answer> and is one of the following:
%
% nr(d):   no domain reduction because the result was larger than the initial
%	   domain.
% nr(f):   no domain reduction because we failed to apply the Interval Newton
%	   operator because of a shared zero between the function range for 
%	   <Variable> in the center of <Interval> and the derivative range for
%	   <Variable> in <Interval>.
% r(D):    domain reduction: the reduced domain is returned in <D>.
% sr(L,R): domain reduction: the reduced domain consists of two parts, returned
%	   in <L> and <R>.
% f:	   failed: no solution for <Variable> in <Interval>.
%
% * The partial evaluation consists of replacing all variables but <Variable>
%   by their domains and performing calculations if possible.

interval_newton(IT,FPE,DPE,V,Interval,Answer) :-
	(   newton_intervals(IT,FPE,DPE,V,Interval,Result)
	->  (   union_intersect(Result,Interval,Intersect)
	    ->  (   Intersect == Interval
		->  Answer = nr(d) % no reduction
		;   Intersect = i(_,_)
		->  Answer = r(Intersect) % reduction
		;   Intersect = u(L,R)
		->  Answer = sr(L,R) % split reduction
		)
	    ;   Answer = f % fail: no solution in <Interval>
	    )
	;   Answer = nr(f) % no reduction: failed to apply interval Newton
	).

% newton_interval(IntervalExtension,FunctionPartialEvaluation,
%	DerivativePartialEvaluation,Variable,Interval,Result)
%
% Computes an Interval Newton step for the function with partially evaluated *
% interval extension <FunctionPartialEvaluation> and partially evaluated *
% interval extension of the derivative <DerivativeIntervalExtension>, with
% respect to variable <Variable> which has initial domain <Interval>. The
% result is returned in <Result> and is either an interval or a union of
% two intervals. No intersection of the result with <Interval> is made in this
% step. This predicate fails if we cannot apply the Interval Newton step
% because of a shared zero between the function range for <Variable> in the
% center of <Interval> and the derivative range for <Variable> in <Interval>.
%
% * The partial evaluation consists of replacing all variables but <Variable>
%   by their domains and performing calculations if possible.

newton_intervals(IT,FPE,DPE,V,Interval,Result) :-
	interval_center(Interval,Center),
	get_full_evaluation(IT,FPE,V,Center,FInt),
	get_full_evaluation(IT,DPE,V,Interval,DInt),
	(   interval_contains(DInt,0)
	->  (   interval_split_excluding_zero(DInt,Left,Right)
	    ->  interval_new_class(FInt,Class),
		(   Class == p
		->  interval_div(FInt,Left,LeftDiv),
		    interval_div(FInt,Right,RightDiv)
		;   Class == n
		->  interval_div(FInt,Left,RightDiv),
		    interval_div(FInt,Right,LeftDiv)
		),
		interval_minus(Center,LeftDiv,RightResult),
		interval_minus(Center,RightDiv,LeftResult),
		(   interval_intersection(LeftResult,RightResult,_)
		->  interval_union(LeftResult,RightResult,Result)
		;   Result = u(LeftResult,RightResult)
		)
	    ;	interval_exclude_zero_bound(DInt,Nonzero),
		\+ interval_contains(FInt,0),
		interval_div(FInt,Nonzero,Div),
		interval_minus(Center,Div,Result)
	    )	
	;   interval_div(FInt,DInt,Div),
	    interval_minus(Center,Div,Result)
	).

% union_intersect(Input,Interval,Intersection)
%
% Intersects the input <Input> which is either an interval or a union of two
% intervals, by the interval <Interval> and returns the result in
% <Intersection> which is again either an interval or a union of two intervals.

union_intersect(X,Y,Z) :-
	(   X = i(_,_)
	->  interval_intersection(X,Y,Z)
	;   X = u(L,R)
	->  (   interval_intersection(L,Y,LInt)
	    ->  (   interval_intersection(R,Y,RInt)
		->  Z = u(LInt,RInt)
		;   Z = LInt
		)
	    ;   interval_intersection(R,Y,Z)
	    )
	).