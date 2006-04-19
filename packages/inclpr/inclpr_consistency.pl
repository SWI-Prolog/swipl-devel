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

:- module(inclpr_consistency,
	[
	    check/8,
	    check_projection/5,
	    create_base_form/4,
	    create_partial_evaluation/5,
	    get_direct_full_evaluation/4,
	    get_full_evaluation/5,
	    instantiate_nat/2,
	    instantiate_all_nat/1,
	    max_reduced/3
        ]).

:- use_module(library(chr)).
:- use_module(inclpr_interval_arithmetic,
	[
    	    accepted_solution_domain/1,
	    minimal_interval/3,
	    eq_subdivide/3,
	    uneq_subdivide/4,
	    interval_center/2,
	    interval_new_class/2
	]).
:- use_module(inclpr_newton,
	[
	    interval_newton/6
	]).
:- use_module(inclpr_natural_interval_extension,
	[
	    n_i_e_create_base_form/3,
	    n_i_e_create_partial_evaluation/4,
	    n_i_e_create_full_evaluation/4,
	    n_i_e_create_direct_full_evaluation/3
        ]).

:- chr_constraint check/8.

:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_option(toplevel_show_store,off).

% Module implementing consistency checking and domain narrowing for
% box-consistency.

% check(IE,FunctionPartialEvaluation,DerivativePartialEvaluation,Relation,
%	Variable,Interval,Direction,NewInterval)
%
% Checks whether the constraint <Function> <Relation> 0 with 
% <FunctionPartialEvaluation> the partial evaluation of <Function> and
% <DerivativePartialEvaluation> the partial evaluation of the derivative of
% <Function>, is box-consistent with respect to variable <Variable> and domain
% of <Variable> = <Interval> in direction <Direction>. <Direction> is either
% the atom `lower' or the atom `upper' and represents which bound of <Interval>
% is checked. <Relation> is either =, =< or >=. If it is not box-consistent,
% domain narrowing is applied until reaching box-consistency. The resulting
% new domain is returned in <NewInterval>.

check(IE,FPE,DPE,Rel,V,Interval,Direction,NewInterval) <=>
	minimal_interval(Direction,Interval,Int),
	(   check_projection(Rel,IE,FPE,V,Int)
	->  NewInterval = Interval
	;   narrow(IE,FPE,DPE,Rel,V,Interval,Direction,Result)
	->  (   Direction == lower
	    ->  Interval = i(LOld,U),
		Result = i(L,_),
		F is L - LOld
	    ;   Interval = i(L,UOld),
		Result = i(_,U),
		F is UOld - U
	    ),
	    NewInterval = i(L,U)
	).
	
% narrow(IE,FunctionPartialEvaluation,DerivativePartialEvaluation,Relation,
%	Variable,Interval,Direction,Result)
%
% Narrows the domain <Interval> of variable <Variable> to reach box-consistency
% for the constraint <Function> <Relation> 0 with <FunctionPartialEvaluation>
% the partial evaluation of <Function> and <DerivativePartialEvaluation> the
% partial evaluation of the derivative of <Function> and in the direction
% <Direction>. <Direction> is either the atom `lower' or the atom `upper' and
% represents which bound of <Interval> is narrowed. <Relation> is either =, =<
% or >=. The resulting new domain is returned in <Result>.

narrow(IE,FPE,DPE,Rel,V,Interval,Direction,Result) :-
	interval_newton(IE,FPE,DPE,V,Interval,Answer),
	(   Answer = r(NewInterval)
	->  narrow(IE,FPE,DPE,Rel,V,NewInterval,Direction,Result)
	;   Answer = nr(Code)
	->  (   accepted_solution_domain(Interval)
	    ->  check_projection((=),IE,FPE,V,Interval),
		Result = Interval
	    ;   minimal_interval(Direction,Interval,MInt),
		check_projection((=),IE,FPE,V,MInt)
	    ->  Result = Interval
	    ;   (   Code == d
		->  eq_subdivide(Interval,Left,Right)
		;   uneq_subdivide(Direction,Interval,Left,Right)
		),
		(   Direction == lower
		->  (   check_projection((=),IE,FPE,V,Left),
		        narrow(IE,FPE,DPE,Rel,V,Left,lower,Result)
		    ->  true
		    ;   check_projection((=),IE,FPE,V,Right),
		        narrow(IE,FPE,DPE,Rel,V,Right,lower,Result)
		    )
		;   (   check_projection((=),IE,FPE,V,Right),
		        narrow(IE,FPE,DPE,Rel,V,Right,upper,Result)
		    ->  true
		    ;   check_projection((=),IE,FPE,V,Left),
		        narrow(IE,FPE,DPE,Rel,V,Left,upper,Result)
		    )
		)
	    )	
	;   Answer = sr(Left,Right),
	    (   Direction == lower
	    ->  (   check_projection((=),IE,FPE,V,Left),
	            narrow(IE,FPE,DPE,Rel,V,Left,lower,Result)
	        ->  true
	        ;   check_projection((=),IE,FPE,V,Right),
		    narrow(IE,FPE,DPE,Rel,V,Right,lower,Result)
	        )
	    ;   (   check_projection((=),IE,FPE,V,Right),
		    narrow(IE,FPE,DPE,Rel,V,Right,upper,Result)
		->  true
		;   check_projection((=),IE,FPE,V,Left),
		    narrow(IE,FPE,DPE,Rel,V,Left,upper,Result)
		)
	    )
	).

% check_projection(Relation,IE,PartialEvaluation,Variable,Domain)
%
% Checks whether the full evaluation of <PartialEvaluation> with respect to
% <Variable> and <Domain>, generated by get_full_evaluation/5, satisfies the
% constraint <FullEvaluation> <Relation> 0. with <Relation> one of =, =< and 
% >=.

check_projection((=),IE,PE,V,D) :-
	get_full_evaluation(IE,PE,V,D,FE),
	interval_new_class(FE,z).
check_projection((=<),IE,PE,V,D) :-
	get_full_evaluation(IE,PE,V,D,FE),
	\+ interval_new_class(FE,p).
check_projection((>=),IE,PE,V,D) :-
	get_full_evaluation(IE,PE,V,D,FE),
	\+ interval_new_class(FE,n).

% max_reduced(IE,BaseForm,Variables)
%
% Checks whether the domains of the variables in <Variables> can be reduced by
% domain narrowing with respect to the constraint with base form <BaseForm> by
% looking at the width of the full evaluation of <BaseForm> with respect to
% interval extension <IE>. This creates a stopping criterion for constraints
% for which the derivative is near zero at a wide range around the solution
% points.

max_reduced(IE,BF,BFV) :-
	get_direct_full_evaluation(IE,BF,BFV,FE),
	accepted_solution_domain(FE).

% get_full_evaluation(IE,PartialEvaluation,Variable,Domain,FullEvaluation)
%
% Computes the full evaluation of <PartialEvaluation> with respect to 
% <Variable> and <Domain> using create_full_evaluation/5, stores the resulting
% interval in a nonbacktrackable way and backtracks the evaluation. This 
% enables us to use the partial evaluation multiple times. The resulting
% interval is returned in <FullEvaluation>.

get_full_evaluation(IE,PE,V,D,FE) :-
	(   create_full_evaluation(IE,PE,V,D,FE),
	    nb_setval(inclpr_eval,FE),
	    fail
	;   nb_getval(inclpr_eval,FE),
	    nb_delete(inclpr_eval)
	).

% get_direct_full_evaluation(IE,BaseForm,Variables,FullEvaluation)
%
% Computes the full evaluation of <BaseForm> with variables <Variables> by
% using create_direct_full_evaluation/4, stores the resulting interval in a
% nonbacktrackable way and backtracks the evaluation. This enables us to use
% the base form multiple times. The resulting interval is returned in
% <FullEvaluation>.

get_direct_full_evaluation(IE,BF,BFV,FE) :-
	(   create_direct_full_evaluation(IE,BF,BFV,FE),
	    nb_setval(inclpr_eval,FE),
	    fail
	;   nb_getval(inclpr_eval,FE),
	    nb_delete(inclpr_eval)
	).

% create_base_form(IE,Function,BaseForm,Variables)
%
% Creates a base form for the interval extension denoted by <IE> of function
% <Function> and returns it into <BaseForm>. The variables of <Function> are
% returned in <Variables>.

create_base_form(n,Term,BF,BFV) :- 
	n_i_e_create_base_form(Term,BF,BFV).

% create_partial_evaluation(IE,BaseForm,Variables,PartialEvaluation,Variable)
%
% Creates a partial evaluation <PartialEvaluation> of the interval extension
% denoted by <IE> with base form <BaseForm>. The variables of the function for
% which <BaseForm> is the base form, are in <Variables>. A partial evaluation
% consists of unifying all domain aliases by the domain of the related
% variable and all center aliases by the center of the domain of the related
% variable except for the variable <Variable>. Partial evaluations are used
% to speed up domain narrowing and consistency checking.

create_partial_evaluation(n,BF,BFV,PE,V) :- 
	n_i_e_create_partial_evaluation(BF,BFV,PE,V).

% create_full_evaluation(IE,PartialEvaluation,Variable,Domain,FullEvaluation)
%
% Creates a full evaluation <FullEvaluation> (i.e. an interval) of the interval
% extension denoted by <IE> with partial evaluation <PartialEvaluation>. This
% is done by unifying the domain alias and the center alias of variable
% <Variable> by <Domain> and the center of <Domain> respectively.

create_full_evaluation(n,PE,V,D,FE) :- 
	n_i_e_create_full_evaluation(PE,V,D,FE).

% create_direct_full_evaluation(IE,BaseForm,Variables,FullEvaluation)
%
% Creates a full evaluation <FullEvaluation> (i.e. an interval) of the interval
% extension denoted by <IE> with base form <BaseForm>. The variables of the
% function for which <BaseForm> is the base form, are in <Variables>. All
% domain aliases and center aliases of the variables in <Variables> are
% unified by the domain respectively the center of the domain of the related
% variable. Direct full evaluations are used for inverted constraints which do
% not benefit from partial evaluations.

create_direct_full_evaluation(n,BF,BFV,FE) :-
	n_i_e_create_direct_full_evaluation(BF,BFV,FE).

% instantiate_nat(VariableList,Variable)
%
% Instantiates all domain aliases of the variables in <VariableList> by the
% domain of the respective variable except for the alias for variable
% <Variable>. This is specific for interval extensions that do are not based on
% center values like the natural interval extension.

instantiate_nat([],_).
instantiate_nat([H|T],Var) :-
	(   Var == H
	->  instantiate_nat(T,Var)
	;   get_attr(H,inclpr_aliases,cd(_,D)),
	    get_attr(H,inclpr_domain,D),
	    instantiate_nat(T,Var)
	).

% instantiate_all_nat(VariableList)
%
% Instantiates all domain aliases of the variables in <VariableList> by the
% domain of the respective variable. This is specific for interval extensions
% that do are not based on center values like the natural interval extension.

instantiate_all_nat([]).
instantiate_all_nat([H|T]) :-
	get_attr(H,inclpr_aliases,cd(_,D)),
	get_attr(H,inclpr_domain,D),
	instantiate_all_nat(T).
