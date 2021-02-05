/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2015, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(arithmetic,
          [ arithmetic_function/1,              % +Name/Arity
            arithmetic_expression_value/2,      % Expression, -Value
            cut_eval/0                          % cut arithmetic_expression_value choices
          ]).
:- autoload(library(error),[type_error/2]).

:- set_prolog_flag(generate_debug_info, false).

/** <module> Extensible arithmetic

This module provides a  portable   partial  replacement  of SWI-Prolog's
user-defined  arithmetic  (evaluable)   functions.    It   defines   the
compatibility  directive  arithmetic_function/1  and  support  for  both
runtime and compile-time evaluation of expressions   that  are a mixture
between Prolog predicates  used  as   functions  and  built-in evaluable
terms.
*/

:- multifile
    evaluable/2.                            % Term, Module

%!  arithmetic_function(:NameArity) is det.
%
%   Declare a predicate as an arithmetic function.
%
%   @deprecated This function provides  a   partial  work around for
%   pure Prolog user-defined arithmetic  functions   that  has  been
%   dropped in SWI-Prolog  5.11.23.  Notably,   it  only  deals with
%   expression know at compile time.

arithmetic_function(Term) :-
    throw(error(context_error(nodirective, arithmetic_function(Term)), _)).

arith_decl_clauses(NameArity, Clauses) :-
    pred_indicator(NameArity,Name,Arity)
     -> 
        compound_name_arity(Term, Name, Arity),  %  for possible 0 arity
        ImplArity is Arity+1,
        functor(Pred, Name, ImplArity),
        prolog_load_context(module, M),
        defining_context(M:Pred,Q),
        PI = Q:Name/ImplArity,
        (evaluable(Term, Q)                      % make idempotent
         -> Clauses=[]
         ;  Clauses=[(:- public(PI)),arithmetic:evaluable(Term, Q)]
        )
     ;  type_error(predicate_indicator, NameArity).

pred_indicator(_:NameArity, Name, Arity) :- % for compatibility - throw away any specified module
    pred_indicator(NameArity, Name, Arity).
pred_indicator(Name/Arity, Name, Arity). 
  
defining_context(Pred,M) :- 
	predicate_property(Pred,implementation_module(M)), !.  % local to M  
defining_context(Pred,C) :- 
	predicate_property(Pred,imported_from(C)), !.          % imported from C          
defining_context(_,user).                                  % not found, sorted out at evaluation? 

%!  eval_clause(+Term, -Clause) is det.
%
%   Clause is a clause  for   evaluating  the  arithmetic expression
%   Term.

eval_clause(roundtoward(_,Round), (eval(Gen,Result) :- Body)) :-
    !,
    Gen = roundtoward(Arg,Round),
    eval_args([Arg], [PlainArg], Goals,
              [Result is roundtoward(PlainArg,Round)]),
    list_conj(Goals, Body).
eval_clause(Term, (eval(Gen, Result) :- Body)) :-
    functor(Term, Name, Arity),
    functor(Gen, Name, Arity),
    Gen =.. [_|Args],
    eval_args(Args, PlainArgs, Goals, [Result is NewTerm]),
    NewTerm =.. [Name|PlainArgs],
    list_conj(Goals, Body).

eval_args([], [], Goals, Goals).
eval_args([E0|T0], [A0|T], [eval(E0, A0)|GT], RT) :-
    eval_args(T0, T, GT, RT).

list_conj([One], One) :- !.
list_conj([H|T0], (H,T)) :-
    list_conj(T0, T).

eval_clause(Clause) :-
    current_arithmetic_function(Term),
    eval_clause(Term, Clause).

term_expansion(eval('$builtin', _), Clauses) :-
    findall(Clause, eval_clause(Clause), Clauses).


%!  arithmetic_expression_value(:Expression, -Result) is det.
%
%   True  when  Result  unifies  with    the  arithmetic  result  of
%   evaluating Expression.

arithmetic_expression_value(Expression, Result) :-
    eval(Expression, Result).

eval(Var, _) :-            % var check to prevent infinite eval loop
    var(Var),
    !, fail.
eval(Number, Number) :-    % first numbers
    number(Number),
    !.
eval(Term, Result) :-      % then user defined functions
    callable_function(Term,Function,Module),
    call(Module:Function, Result),  % possibly multiple choices
    !.                              % late cut allows overloading
eval(Literal, Literal) :-  % then other literals - evaluate to themselves
    (atom(Literal)
     -> \+ current_arithmetic_function(Literal)  % not builtin (family of pi and e)
     ;  atomic(Literal)    % others - strings, [], ..
    ),
    !.
eval('$builtin', _).       % then builtin arithmetic (expanded above)

% used by eval/2
callable_function(Atom, Function, Module) :-
    atom(Atom),
    compound_name_arity(Function,Atom,0),
    evaluable(Function, Module).
callable_function(Function,Function,Module) :-
    compound(Function),
    evaluable(Function, Module).

% used by do_expand_function/3	
callable_function_arguments(Atom, Atom, []) :-
    atom(Atom),
    compound_name_arguments(Function,Atom,[]),
    evaluable(Function, _).
callable_function_arguments(Function, Name, Args) :-
    evaluable(Function, _),
    Function=..[Name|Args].

%
% cut_eval/0 - used by user defined functions to cut eval/2 choices
% necessary to force failure in such predicates (see test suite)
%
cut_eval :-
	ancestor_cut(arithmetic:eval(_,_)).

ancestor_cut(Goal) :-  % see "Hackers corner"
    prolog_current_choice(C),
    ancestor_cut(Goal, C).
ancestor_cut(Goal, C) :-
    prolog_choice_attribute(C,parent,C1),
    prolog_choice_attribute(C,frame,F),
    (prolog_frame_attribute(F,goal,G), G=Goal
     -> prolog_cut_to(C1)
     ;  ancestor_cut(Goal, C1)
    ).
			
                 /*******************************
                 *         COMPILE-TIME         *
                 *******************************/

math_goal_expansion(A is Expr, Goal) :-
    expand_function(Expr, Native, Pre),
    tidy((Pre, A is Native), Goal).
math_goal_expansion(ExprA =:= ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA =:= NativeB), Goal).
math_goal_expansion(ExprA =\= ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA =\= NativeB), Goal).
math_goal_expansion(ExprA > ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA > NativeB), Goal).
math_goal_expansion(ExprA < ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA < NativeB), Goal).
math_goal_expansion(ExprA >= ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA >= NativeB), Goal).
math_goal_expansion(ExprA =< ExprB, Goal) :-
    expand_function(ExprA, NativeA, PreA),
    expand_function(ExprB, NativeB, PreB),
    tidy((PreA, PreB, NativeA =< NativeB), Goal).

expand_function(Expression, NativeExpression, Goal) :-
    do_expand_function(Expression, NativeExpression, Goal0),
    tidy(Goal0, Goal).

do_expand_function(X, X, true) :-              % #1 anything evaluable
    evaluable(X),
    !.
do_expand_function(roundtoward(Expr0, Round),  % #2 roundtoward special case
                   roundtoward(Expr, Round),
                   ArgCode) :-
    !,
    do_expand_function(Expr0, Expr, ArgCode).
do_expand_function(Function,                   % #3 user defined (before built in for overloading)
                   Result,
                   (ArgCode, arithmetic_expression_value(Pred,Result))) :-
	callable_function_arguments(Function,Name,Args),
    !,
    expand_predicate_arguments(Args, PredArgs, ArgCode),
    Pred =.. [Name|PredArgs].
do_expand_function(Function,                   % #4  builtin (before atomic for family of pi)
                   Result,
                   ArgCode) :-
	callable(Function),  % guard before
    current_arithmetic_function(Function),
    !,
    Function =.. [Name|Args],
    expand_function_arguments(Args, ArgResults, ArgCode),
    Result =.. [Name|ArgResults].
do_expand_function(X, Result, Result=X) :-     % #5 other literals, move out of expression
    atomic(X),
    !.
do_expand_function(Function, _, _) :-          % #6 WTF?
    type_error(evaluable, Function).


expand_function_arguments([], [], true).
expand_function_arguments([H0|T0], [H|T], (A,B)) :-
    do_expand_function(H0, H, A),
    expand_function_arguments(T0, T, B).

expand_predicate_arguments([], [], true).
expand_predicate_arguments([H0|T0], [H|T], (A,B)) :-
    do_expand_function(H0, H1, A0),
    (   callable(H1),
        current_arithmetic_function(H1)
    ->  A = (A0, H is H1)
     ;  (A0 = (X=R) -> X=R, A=true ; A = A0),  % optimization for atomics
        H = H1
    ),
    expand_predicate_arguments(T0, T, B).

%!  evaluable(F) is semidet.
%
%   True if F and all its subterms are variables or evaluable terms by builtin functions.
%
evaluable(F) :-
    var(F),
    !.
evaluable(F) :-
    number(F),
    !.
evaluable([Code]) :- 
    % assumes possibility of future environment flag to disable
	(current_prolog_flag(disable_codeTBD,true) -> fail ; eval_code(Code)),
	!.
evaluable(Func) :-                   % Functional notation.
    functor(Func, ., 2),
    !.
evaluable(F) :-                      % unfortunate case - should be a literal
    string(F),
    !,
    string_length(F, 1).
evaluable(roundtoward(F,_Round)) :-  % special case to ignore atom(_Round)
    !,
    evaluable(F).
evaluable(F) :-
    current_arithmetic_function(F),
    \+ evaluable(F,_),               % ** not overridden **
    (   compound(F)
    ->  forall(arg(_,F,A), evaluable(A))
    ;   true
    ).

% as defined by builtin
eval_code(Code) :- var(Code).
eval_code(Code) :- integer(Code), Code>=0.
eval_code(Code) :- atom(Code), atom_length(Code,1).

%!  tidy(+GoalIn, -GoalOut)
%
%   Cleanup the output from expand_function/3.

tidy(A, A) :-
    var(A),
    !.
tidy(((A,B),C), R) :-
    !,
    tidy((A,B,C), R).
tidy((true,A), R) :-
    !,
    tidy(A, R).
tidy((A,true), R) :-
    !,
    tidy(A, R).
tidy((A, X is Y), R) :-
    var(X), var(Y),
    !,
    tidy(A, R),
    X = Y.
tidy((A,B), (TA,TB)) :-
    !,
    tidy(A, TA),
    tidy(B, TB).
tidy(A, A).


                 /*******************************
                 *        EXPANSION HOOK        *
                 *******************************/

:- multifile
    system:term_expansion/2,
    system:goal_expansion/2.

system:term_expansion((:- arithmetic_function(Term)), Clauses) :-
    arith_decl_clauses(Term, Clauses).

system:goal_expansion(Math, MathGoal) :-
    math_goal_expansion(Math, MathGoal).
