/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2008-2011, University of Amsterdam
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

:- module(test_strace,
	  [ test_strace/0
	  ]).
:- use_module(library(debug)).

/** <module> Test stack-fetching primitives

This test verifies correct functioning  of $clause_term_position/3 under
all possible control primitives. It is   related  to test_gtrace.pl, but
follows a different route to check the clauses and exploits knowledge of
the implementation of $clause_term_position/3 to minimise the testing.

In particular, not the test-cases for   if-then. This is because finding
if-then implies finding the C_END instruction.

@tbd	Possibly we should add another (dummy) argument to C_IFTHEN
	to find the end of the construct and simplify this and the
	decompiler?
*/

% :- debug(cont).

test_strace :-
	chk(c(_)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Structures:

	conjunction
	disjunction
	if-then-else
	if-then
	not
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- '$clausable'(c/1).
c(conj1) :- a.
c(conj2) :- a,b,c.
c(disj1) :- (a;b).
c(disj2) :- (a;b;c).
c(ite1)  :- (a->b;c).
c(it1)   :- (a->b).
c(it2)   :- (a->b,c).
c(it3)   :- (a->(b;c)).
c(it4)   :- (a->(b->c;d)).
c(it5)   :- (a->(b->c)).
c(it6)   :- (a->(\+b)).
c(not1)  :- \+a.

chk(Head) :-
	forall((nth_clause(Head, _, CRef),
		clause(Head, _, CRef)),
	       check_clause(Head, CRef)).

check_clause(Head, CRef) :-
	debug(cont, 'Checking ~w (~w)', [Head, CRef]),
	forall('$break_pc'(CRef, Start, Cont),
	       check_cont(CRef, Start, Cont)).

check_cont(CRef, Start, Cont) :-
	'$fetch_vm'(CRef, Start, NextPC, VMI),
	assertion(NextPC == Cont),
	debug(cont, '~w: ~w', [Cont, VMI]),
	'$clause_term_position'(CRef, Cont, List),
	debug(cont, '\t ~w', [List]),
	(   List == exit		% on final exit of clause
	->  true
	;   term_in_clause(CRef, List, Term)
	->  debug(cont, '\t--> ~w', [Term]),
	    ok_literal(VMI, Term)
	;   fail
	).


term_in_clause(CRef, List, Term) :-
	clause(Head, Body, CRef),
	find_term(List, (Head:-Body), Term).

ok_literal(i_enter, _).			% Head: not much point checking
ok_literal(i_call(_:X/0), X).
ok_literal(i_depart(_:X/0), X).


find_term([], Term, Term).
find_term([H|T], Term0, Term) :-
	arg(H, Term0, Term1),
	find_term(T, Term1, Term).


a.
b.
c.
d.


