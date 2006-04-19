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
:- module(inclpr_natural_interval_extension,
	[
	    n_i_e_create_base_form/3,
	    n_i_e_create_partial_evaluation/4,
	    n_i_e_create_full_evaluation/4,
	    n_i_e_create_direct_full_evaluation/3
	]).

:- use_module(inclpr_interval_arithmetic,
	[
	    ia_binary_operator/4,
	    ia_eval/2,
	    ia_eval_g/2,
	    ia_unary_operator/3,
	    interval/1
	]).
:- use_module(inclpr_core,
	[
	    get_domain/2
	]).
:- use_module(inclpr_consistency,
	[
	    instantiate_nat/2,
	    instantiate_all_nat/1
	]).

% Module for creating evaluations of the natural interval extension.

% n_i_e_create_base_form(Function,BaseForm,Variables)
%
% Creates a base form of function <Function> for calculating the natural
% interval extension of <Function>. The base form is made by replacing all
% variable occurrences by the variable alias for the domain and by computing
% the result of ground subterms. The base form is returned in <BaseForm> and
% the variables in <Function> are returned in <Variables>.

n_i_e_create_base_form(Term,BaseForm,BaseFormVars) :-
	n_i_e_create_base_form_int(Term,BaseForm),
	term_variables(Term,BaseFormVars).

% n_i_e_create_base_form_int(Function,BaseForm)
%
% Internal predicate for creating the base form of function <Function> which
% is stored in <BaseForm>. See n_i_e_create_base_form/3.

n_i_e_create_base_form_int(Term,BaseForm) :-
	(   var(Term)
	->  get_attr(Term,inclpr_aliases,cd(_,BaseForm))
	;   number(Term)
	->  BaseForm = i(Term,Term)
	;   Term = i(_,_)
	->  BaseForm = Term
	;   functor(Term,Op,Arity),
	    (   Arity =:= 2
	    ->  arg(1,Term,L),
		arg(2,Term,R),
		n_i_e_create_base_form_int(L,LBF),
		n_i_e_create_base_form_int(R,RBF),
		ia_binary_operator(Op,LBF,RBF,BaseForm)
	    ;   arg(1,Term,X),
		n_i_e_create_base_form_int(X,XBF),
		ia_unary_operator(Op,XBF,BaseForm)
	    )
	).

% n_i_e_create_partial_evaluation(BaseForm,Variables,PartialEvaluation,
%	variable)
%
% Creates a partial evaluation of the base form <BaseForm> by replacing all
% domain aliases of variables in <Variables> by the domain of the respective
% variable, except for variable <Variable>. The results of ground subterms in
% the partial evaluation are calculated. Partial evaluations are used to speed
% up consistency checks and domain narrowing, since only one variable changes
% its domain for these operations.
 
n_i_e_create_partial_evaluation(BaseForm,BaseFormVars,PartialEvaluation,Var) :-
	instantiate_nat(BaseFormVars,Var),
	ia_eval(BaseForm,PartialEvaluation).

% n_i_e_create_full_evaluation(PartialEvaluation,Variable,Domain,
%	FullEvaluation)
%
% Creates in <FullEvaluation> a full evaluation (i.e. an interval) from the
% partial evaluation <PartialEvaluation> by replacing the only uninstantiated
% domain alias, namely that of variable <Variable> by <Domain>.

n_i_e_create_full_evaluation(PartialEvaluation,Var,Domain,FullEvaluation) :-
	get_attr(Var,inclpr_aliases,cd(_,Domain)),
	ia_eval_g(PartialEvaluation,FullEvaluation).

% n_i_e_create_direct_full_evaluation(BaseForm,BaseFormVars,FullEvaluation)
%	
% Creates in <FullEvaluation> a full evaluation (i.e. an interval) of the base
% form <BaseForm> by replacing all domain aliases of variables in <Variables>
% by the domain of the respective variable and computing the result of the
% resulting ground expression. This predicate is used for inverted constraints,
% since they only require one evaluation per domain checking/narrowing, which
% makes partial evaluations unnecessary.

n_i_e_create_direct_full_evaluation(BF,BFV,FE) :-
	instantiate_all_nat(BFV),
	ia_eval_g(BF,FE).