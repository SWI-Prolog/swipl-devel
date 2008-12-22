/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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


