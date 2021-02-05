/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           www.swi-prolog.org
    Copyright (c)  2011, University of Amsterdam
                         VU University Amsterdam
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

:- module(test_arithfunc,
	  [ test_arithfunc/0
	  ]).
:- use_module(library(plunit)).
:- if(exists_source(library(arithmetic))).
:- use_module(library(arithmetic)).

%%	test_arithfunc
%
%	Test  emulation  of  arithmetic_function/1    for  compatibility
%	reasons.

test_arithfunc :-
	run_tests([ arithmetic_function
		  ]).

:- begin_tests(arithmetic_function).

:- arithmetic_function(ten/0).
:- arithmetic_function(twice/1).
:- arithmetic_function(mean/2).
:- arithmetic_function(euler/0).
:- arithmetic_function(fail/0).
:- arithmetic_function(except/0).
:- arithmetic_function(fac/1).
:- arithmetic_function(atom_length/1).

ten(10).
twice(X, R) :-
	R is X * 2.
mean(X1, X2, R) :-
	R is (X1 + X2)/2.

euler(2.718281828459045).

fail(_) :-  
    cut_eval,  % requires ancestor cut or evaluates to atom(fail).
    fail.

except(_) :-
	throw(error(foobar)).

fac(1,1) :- !.
fac(X,N) :-
	X > 1,
	X2 is X - 1,
	fac(X2, N0),
	N is N0 * X.

test(func, A == 10) :-
	A is ten.
test(func, A == 10) :-
	A is twice(5).
test(func, A == 10) :-
	A is mean(0, 20).
test(euler, EE =:= 6*e*7*1) :-
        EE is 6*euler*7*1.
test(fail, fail) :- 
	_A is fail.
test(except, throws(error(foobar))) :-
	_ is except.
test(fac, A =:= 3628800) :-
	A is fac(10).
%test(flag, NV == 50) :-
%	flag(f, Old, 100),
%	flag(f, V, mean(V, 0)),
%	flag(f, NV, Old).

% user defined functions aren't dynamic
test(func, throws(error(type_error(evaluable, ten/0), _))) :-
	E = ten, _A is E.

%
% atomics evaluate to themselves
%
test(func, X==[]) :-
    X is [].

test(func, X=="abcd") :-
    X is "abcd".

test(func, X==abcd) :-
    X is abcd.

%
% imported function
%
test(importedfunction) :-
    3 is atom_length(abc).
    
%
% arithmetic_expression_value/2
%    
test(arith_eval, A == 10) :-
    arithmetic_expression_value(10,A).

test(arith_eval, A == 20) :-
    arithmetic_expression_value(twice(5)+10,A).
	
test(arith_eval, A == "ten") :-
    arithmetic_expression_value("ten",A).

test(arith_eval, A =:= pi) :-
    arithmetic_expression_value(pi,A).

test(arith_eval, fail) :-
    arithmetic_expression_value(_,_).


:- if(current_prolog_flag(bounded, false)). % GMP implies rational

:- arithmetic_function(idiv/2).

idiv(Dd,Dr,Iq):-
        Q is Dd/Dr,
        rational(Q,Qt,Qn),
        Iq is Qt//Qn.

test(idiv, Qi == 3) :-
	Qi is idiv(3 rdiv 2,2 rdiv 5).

:- endif.

% overload test

:- arithmetic_function((/\)/2).

/\(S1,S2,R) :- string(S1), string(S2), !,   % string case
    string_concat(S1,S2,R).
/\(S1,S2,R) :-                              % default case (can't be optimized)
	Exp = S1 /\ S2, R is Exp.               % to avoid math_goal_expansion
   
test(func_overload, X==160) :-
	X is 0xAA /\ 0xF0.

test(func_overload, X=="0xAA0xF0") :-
	X is "0xAA" /\ "0xF0".

% Note: '/\' left overloaded after test.
% One way of removing it is by unloading this file. Something like:
% ?- module_property(plunit_arithmetic_function,file(F)), unload_file(F).

:- end_tests(arithmetic_function).

:- else.

test_arithfunc.

:- endif.
