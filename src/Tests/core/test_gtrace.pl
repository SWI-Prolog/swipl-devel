/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2013, University of Amsterdam
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

:- module(test_gtrace,
	  [ test_gtrace/0
	  ]).
:- use_module(library(debug)).
:- use_module(library(lists)).

/** <module> Test support routines for the source-level debugger

This test module verifies correct functioning  of the support predicates
for the graphical debugger, notably:

    * '$break_pc'(ClauseRef, PC, NextPC)
    Find possible break-points in a clause

    * '$clause_term_position'(ClauseRef, NextPC, Path)
    Find the location of a subterm of the clause term that is just
    before NextPC.

@tbd	This is just the first step.  Other tests must verify that the
	debugger is called at the proper locations.
*/


		 /*******************************
		 *	     TEST CASES		*
		 *******************************/

:- '$clausable'(test_clause/2).
					% control structures
test_clause(1, target) :- target.
test_clause(2, target) :- true, target.
test_clause(3, target) :- true, target, fail.
test_clause(or_a, target) :- ( target ; a ).
test_clause(or_b, target) :- ( a ; target ).
test_clause(or_c, target) :- ( a ; b ), target.
test_clause(not_1, target) :- \+ target, a.
test_clause(not_2, target) :- \+ a, target.
test_clause(ite_cond, target) :- ( target -> a ; b ).
test_clause(ite_if, target) :- ( a -> target ; b ).
test_clause(ite_else, target) :- ( a -> b ; target ).
test_clause(ite_z, target) :- ( a -> b ; c ), target.
test_clause(it_if, target) :- ( target -> a ).
test_clause(it_else, target) :- ( a -> target ).
test_clause(it_z, target) :- ( a -> b ), target.
					% identify unification subgoals
test_clause(unify_ff, target) :- A=B, target, a(A), b(B).
test_clause(unify_fv, target) :- b(B), A=B, target, a(A).
test_clause(unify_vv, target) :- a(A), b(B), A=B, target.
test_clause(unify_fc, target) :- A=c, target, a(A).
test_clause(unify_vc, target) :- a(A), A=c, target.
test_clause(unify_exit, target) :- A=c(x), target, a(A).
					% identify normal arithmetic subgoals
test_clause(a_add_fc, target) :- b(B), A is B+1, target, a(A).
					% identify optimized arithmetic subgoals
test_clause(a_firstvar_is, target) :- A is 1+1, target, a(A).
test_clause(a_is, target) :- a(A), A is 1+1, target.


% predicates called by our test clauses.  Just to keep the
% cross-referencer quiet.

target.					% the thing we are normally looking for
a.					% auxiliary preds
b.
c.
a(_).					% auxiliary preds that accesses a variable,
b(_).					% with determines singleton/first/normal cases


		 /*******************************
		 *	       ENTRY		*
		 *******************************/

:- dynamic
	error/1.

test_gtrace :-
	retractall(error(_)),
	(   nth_clause(test_clause(_,_), _N, Ref),
	    (	debugging(break)
	    ->	clause(Head, _, Ref),
		arg(1, Head, Name),
		format(user_error, ' [~w]', [Name])
	    ;	put_char(user_error, '.')
	    ),
	    check_break(Ref),
	    fail
	;   \+ error(_)
	).

check_break(Ref) :-
	clause(Head, Body, Ref),
	arg(1, Head, Name),
	arg(2, Head, Target),
	subgoal_location(Target, Body, BodyPath),
	append([2], BodyPath, OkPath),
	'$break_pc'(Ref, PC, _NextPC1),
	'$fetch_vm'(Ref, PC, NextPC, Instr),
	calls(Instr, Target), !,
	debug(break, 'pc=~d', [PC]),
	'$clause_term_position'(Ref, NextPC, CrPath),
	(   CrPath == OkPath
	->  true
	;   format('~N~w: Subclause location mismatch:~n\c
		    \tok: ~q~n\c
		    \tfound: ~q~n', [Name, OkPath, CrPath]),
	    assert(error(Name))
	).


%%	subgoal_location(+SubTerm, +Term, -Path:list(integer)) is semidet.

subgoal_location(SubTerm, Term, Path) :-
	(   SubTerm =@= Term
	->  Path = []
	;   compound(Term),
	    Path = [I|T],
	    arg(I, Term, Arg),
	    subgoal_location(SubTerm, Arg, T)
	).

calls(i_depart(Proc), Term) :-
	calls(i_call(Proc), Term).
calls(i_call(Name/Arity), Term) :-
      functor(Term, Name, Arity).
calls(i_call(Module:Name/Arity), Module:Term) :-
      functor(Term, Name, Arity).
