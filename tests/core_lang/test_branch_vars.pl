/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
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

:- module(test_branch_vars,
	  [ test_branch_vars/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(lists)).

/** <module> Test sharing frame slots between branches

Variables that only appear in one alternative of a disjunction may share
their frame slot with variables of other alternatives.  This must not
change the semantics and clause/2 must return a variant of the original
clause, also after saving and loading as .qlf file.
*/

test_branch_vars :-
	run_tests([ branch_vars
		  ]).

:- dynamic b/2.

clauses([ (b(1, R) :- ( R > 0 -> A is R*2, p(A) ; B is R+1, p(B) ), q(R)),
	  (b(2, R) :- ( r(R, A), p(A) ; r(R, B), p(B) ; r(R, C), p(C) )),
	  (b(3, R) :- ( R = a, Y = 1 ; R = b, Z = 2, Y = Z ), p(Y)),
	  (b(4, R) :- ( ( q(V) ; r(R, _) ), p(V) ; q(R) )),
	  (b(5, R) :- ( member(A, [1,2]), garbage_collect, R = A
		      ; member(B, [3,4]), garbage_collect, R = B
		      )),
	  (b(6, R) :- ( member(A, [1,2]), R = x(A)
		      ; ( member(B, [a,b]) *-> R = y(B) ; R = z )
		      ; \+ member(_, [1]), R = n
		      ; R = last
		      ))
	]).

p(_).
q(_).
r(X, X).

load_clauses :-
	retractall(b(_,_)),
	clauses(Clauses),
	forall(member(C, Clauses), assertz(C)).

variant_clause(M, Clause) :-
	copy_term(Clause, Copy),
	Copy = (Head :- _),
	clause(M:Head, Body),
	(Head :- Body) =@= Clause.

:- begin_tests(branch_vars, [setup(load_clauses)]).

test(variant) :-
	clauses(Clauses),
	forall(member(C, Clauses), variant_clause(test_branch_vars, C)).
test(solutions, Sols == [1,2,3,4]) :-
	findall(R, b(5, R), Sols).
test(solutions, Sols == [x(1),x(2),y(a),y(b),last]) :-
	findall(R, b(6, R), Sols).
test(qlf, Sols == [x(1),x(2),y(a),y(b),last]) :-
	tmp_file(branch_vars, Base),
	file_name_extension(Base, pl, File),
	file_name_extension(Base, qlf, QlfFile),
	clauses(Clauses),
	setup_call_cleanup(
	    open(File, write, Out),
	    ( format(Out, ':- module(branch_vars_qlf, []).~n', []),
	      forall(member(C, Clauses), portray_clause(Out, C)),
	      forall(member(H, [p(_), q(_), r(_,_)]),
		     forall(clause(H, B), portray_clause(Out, (H:-B))))
	    ),
	    close(Out)),
	call_cleanup(
	    ( qcompile(File),
	      load_files(QlfFile, [if(true)]),
	      forall(member(C, Clauses), variant_clause(branch_vars_qlf, C)),
	      findall(R, branch_vars_qlf:b(6, R), Sols)
	    ),
	    ( delete_file(File),
	      delete_file(QlfFile)
	    )).

:- end_tests(branch_vars).
