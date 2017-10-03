/*  Part of SWI-Prolog

    Author:        Benoit Desouter <Benoit.Desouter@UGent.be>
		   Jan Wielemaker (SWI-Prolog port)
    Copyright (c)  2016-2017, Benoit Desouter and Jan Wielemaker
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

:- module(test_tabling,
	  [ test_tabling/0
	  ]).
:- use_module(library(tabling)).
:- use_module(tabling_testlib).
:- use_module(library(plunit)).
:- use_module(library(debug)).

test_tabling :-
	run_tests([ tabling_ex1,
		    tabling_ex2,
		    tabling_ex3,
		    tabling_ex4,
		    tabling_ex5,
		    tabling_ex6,
		    tabling_ex7,
		    tabling_ex8,
		    tabling_ex9a,
		    tabling_ex9b,
		    tabling_ex9c,
		    tabling_ex9d,
		    tabling_ex9e,
		    tabling_ex10,
		    tabling_ex11,
		    tabling_ex12,
		    tabling_ex13,
		    tabling_ex14,
		    tabling_ex15,
		    tabling_ex16,
		    tabling_ex17,
		    tabling_clpdf,
		    tabling_eruption,
		    tabling_sneezing,
		    tabling_yappath,
		    tabling_minpath,
		    tabling_train,
		    moded_tabling_path
		  ]).

		 /*******************************
		 *	    EXAMPLE 1		*
		 *******************************/

:- begin_tests(tabling_ex1, [cleanup(abolish_all_tables)]).

expected_variants([a(2,_),a(3,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(2,_),[a(2,3)]).
expected_answers_for_variant(a(3,_),[]).

a_expected_answers([1-2,2-3,1-3]).

a_compare_answers :-
  compare_real_expected_answers(a,2,a_expected_answers),
  test_expected_variants_present,
  test_answers_expected_tables.

:- table a/2.

a(X,Y) :- before, a(X,Z), between, a(Z,Y).
a(X,Y) :- e(X,Y).

e(1,2).
e(2,3).

test(ex1) :-
	a_compare_answers.

:- end_tests(tabling_ex1).

		 /*******************************
		 *           EXAMPLE 2		*
		 *******************************/

:- begin_tests(tabling_ex2, [cleanup(abolish_all_tables)]).
% Meerdere recursieve calls met zelfde callpattern:
% b(X,Y) :- b(X,Z), b(Q,Y).
% For two given facts e(1,2) and e(2,3), there are four (!) solutions to b(X,Y):
% 1,2
% 2,3
% 1,3
% 2,2

expected_variants([b(_,_)]).
expected_answers_for_variant(b(_,_),[b(1,2),b(2,3),b(1,3),b(2,2)]).

% The answers we expect for example 2, returned as a list with entries of the form X-Y. The order does not matter.
b_expected_answers([1-2,2-3,1-3,2-2]).

% TEST: Tests answers of example 2.
b_compare_answers :-
  compare_real_expected_answers(b,2,b_expected_answers).

go :-
  once(b(_X,_Y)).

:- table b/2.

b(X,Y) :- before, b(X,_Z), between, b(_Q,Y).
b(X,Y) :- e(X,Y).

% Test facts
e(1,2).
e(2,3).

test(ex2) :-
	b_compare_answers,
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex2).

		 /*******************************
		 *           EXAMPLE 3		*
		 *******************************/

:- begin_tests(tabling_ex3, [cleanup(abolish_all_tables)]).
% Answers: all integers between -10 and 10 (included).
%
% Consider the execution of the query {\tt ?- p$_g$(Y).} which
% succeeds for all integers between {\tt -10} and {\tt 10} (included)
% against the following tabled program:
% :- table p/1. \\
% p(X) :- p$_{c_1}$(Y), 0 =< Y, Y < 10, X is -Y - 1.  \\
% p(X) :- p$_{c_2}$(Y), -10 < Y, Y =< 0, X is -Y + 1. \\
% p(0).
% The two consumers that are encountered have been given an index for
% ease of reference.  The abstract machine needs to alternate between
% consumers {\tt p$_{c_1}$(Y)} and {\tt p$_{c_2}$(Y)} multiple times
% before all answers have been generated.

expected_variants([c(_)]).
expected_answers_for_variant(c(_),L) :-
  findall(c(X),between(-10,10,X),L).

c_expected_answers(L) :-
  findall(X,between(-10,10,X),L).

c_compare_answers :-
  compare_real_expected_answers(c,1,c_expected_answers).

:- table c/1.

c(X) :- before(1), c(Y), feedback('after recusive 1: Y is ~w, and X is ~w',[Y,X]),
	0 =< Y, Y < 10, X is -Y-1, end(1).
c(X) :- before(2), c(Y), feedback('after recusive 2: Y is ~w, and X is ~w',[Y,X]),
	-10 < Y, Y =< 0, X is -Y+1, end(2).
c(0).

test(ex3) :-
	c_compare_answers,
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex3).


		 /*******************************
		 *           EXAMPLE 4		*
		 *******************************/

:- begin_tests(tabling_ex4, [cleanup(abolish_all_tables)]).
% Two mutually recursive predicates:
% d(X) :- e(Y), Y < 5, X is Y + 1.
% d(0).
%
% e(X) :- d(Y), Y < 5, X is Y + 1.
% e(0).

expected_variants([d(_), e(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).

de_expected_answers(L) :-
  findall(X,between(0,5,X),L).

:- table d/1, e/1.

d(X) :-
	feedback('d/1: before calling e(Y)'),
	e(Y),
	feedback('d/1: after calling e(Y)'),
	feedback('d/1: Y is ~w~n',[Y]),
	Y < 5,
	feedback('d/1: Y < 5 OK\n'),
	(   X is Y + 1
	->  feedback('d/1: is OK\n')
	;   feedback('d/1: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('d/1: X is ~w~n', [X]).
d(0).

e(X) :-
	feedback('e/1: before calling d(Y)'),
	d(Y),
	feedback('e/1: after calling d(Y)'),
	feedback('e/1: Y is ~w~n',[Y]),
	Y < 5,
	feedback('e/1: Y < 5 OK\n', []),
	(   X is Y + 1
	->  feedback('e/1: is OK\n',[])
	;   feedback('e/1: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('e/1: X is ~w~n',[X]).
e(0).

test(ex4) :-
  compare_real_expected_answers(d,1,de_expected_answers),
  compare_real_expected_answers(e,1,de_expected_answers),
  test_expected_variants_present,
  test_answers_expected_tables.

:- end_tests(tabling_ex4).


		 /*******************************
		 *           EXAMPLE 5		*
		 *******************************/

:- begin_tests(tabling_ex5, [cleanup(abolish_all_tables)]).
% Variation on example 1 but with a  cycle. This is an important example
% for tabling.

expected_variants([f(1,_),f(2,_),f(3,_),f(_,_)]).
expected_answers_for_variant(f(_,_),L) :-
  findall(f(X,Y),(between(1,3,X),between(1,3,Y)),L).
expected_answers_for_variant(f(1,_),L) :-
  findall(f(1,X),between(1,3,X),L).
expected_answers_for_variant(f(2,_),L) :-
  findall(f(2,X),between(1,3,X),L).
expected_answers_for_variant(f(3,_),L) :-
  findall(f(3,X),between(1,3,X),L).

% The answers we expect for example 5,   returned as a list with entries
% of the form X-Y. The order does not matter. We expect nine answers.
f_expected_answers([1-1,1-2,1-3,2-1,2-2,2-3,3-1,3-2,3-3]).

:- table f/2.

f(X,Y) :- before, f(X,Z), between, f(Z,Y).
f(X,Y) :- e2(X,Y).

e2(1,2).
e2(2,3).
e2(3,1).

test(ex5) :-
	findall(_,f(_,_),_),
	compare_real_expected_answers(f,2,f_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex5).


		 /*******************************
		 *           EXAMPLE 6		*
		 *******************************/

:- begin_tests(tabling_ex6, [cleanup(abolish_all_tables)]).
% Nested tabling: a tabled predicate calls another tabled predicate, but not mutually recursive.
% g(X) <- h(Y), X is Y + 1.
%
% h(X) <- h(Y), X is Y + 1, X < 5.
% h(0).

expected_variants([g(_),h(_)]).
expected_answers_for_variant(g(_),L) :-
  findall(g(X),between(1,5,X),L).
expected_answers_for_variant(h(_),L) :-
  findall(h(X),between(0,4,X),L).

g_expected_answers([1,2,3,4,5]).
h_expected_answers([0,1,2,3,4]).

:- table g/1, h/1.

g(X) :-
	feedback('g_aux: before calling h(Y)'),
	h(Y),
	feedback('g_aux: after calling h(Y)'),
	X is Y + 1.

h(X) :-
	feedback('h_aux: before calling h(Y)'),
	h(Y),
	feedback('h_aux: after calling h(Y)'),
	X is Y + 1,
	X < 5.
h(0).

test(ex6) :-
	compare_real_expected_answers(g,1,g_expected_answers),
	compare_real_expected_answers(h,1,h_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex6).


		 /*******************************
		 *           EXAMPLE 7		*
		 *******************************/

:- begin_tests(tabling_ex7, [cleanup(abolish_all_tables)]).
% A more complicated graph example:
%
%       i1
%       |
%    +--+---+
%    |      |
%    v      v
%    i2     i3
%    |
% +--+---+
% |      |
% v      v
% i4 --> i5
%
% i1(X) :- i2(Y), X is Y + 1.
% i1(X) :- i3(Y), X is Y + 1.
%
% i2(X) :- i4(Y), X is Y + 1.
% i2(X) :- i5(Y), X is Y + 4. % !! + 4
%
% i4(X) :- i5(Y), X is Y + 1. % !! TABLE table7d
%
% i3(X) :- between(0,1,X). % !! TABLE table7c
%
% i5(Y) :- between(2,3,X).

expected_variants([i1(_),i2(_),i3(_),i4(_),i5(_)]).
expected_answers_for_variant(i1(_),[i1(5),i1(6),i1(7),i1(8),i1(1),i1(2)]).
expected_answers_for_variant(i2(_),[i2(4),i2(5),i2(6),i2(7)]).
expected_answers_for_variant(i3(_),[i3(0),i3(1)]).
expected_answers_for_variant(i4(_),[i4(3),i4(4)]).
expected_answers_for_variant(i5(_),[i5(2),i5(3)]).

i1_expected_answers([5,6,7,8,1,2]).
i2_expected_answers([4,5,6,7]).
i3_expected_answers([0,1]).
i4_expected_answers([3,4]).
i5_expected_answers([2,3]).

:- table i1/1, i2/1, i3/1, i4/1, i5/1.

i1(X) :-
	feedback('i1_aux 1: before'), i2(Y),
	feedback('i1_aux 1: after'),  X is Y + 1.
i1(X) :-
	feedback('i1_aux 2: before'), i3(Y),
	feedback('i1_aux 2: after'),  X is Y + 1.
i2(X) :-
	feedback('i2_aux 1: before'), i4(Y),
	feedback('i2_aux 1: after'),  X is Y + 1.
i2(X) :-
	feedback('i2_aux 2: before'), i5(Y),
	feedback('i2_aux 2: after'),  X is Y + 4.
i4(X) :-
	feedback('i4_aux 1: before'), i5(Y),
	feedback('i4_aux 1: after'),  X is Y + 1.
i3(X) :- between(0,1,X).
i5(X) :- between(2,3,X).

test(ex7) :-
	compare_real_expected_answers(i1,1,i1_expected_answers),
	compare_real_expected_answers(i2,1,i2_expected_answers),
	compare_real_expected_answers(i3,1,i3_expected_answers),
	compare_real_expected_answers(i4,1,i4_expected_answers),
	compare_real_expected_answers(i5,1,i5_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex7).


		 /*******************************
		 *           EXAMPLE 8		*
		 *******************************/

:- begin_tests(tabling_ex8, [cleanup(abolish_all_tables)]).
% Example specifically designed  to  test  whether   we  can  get  a new
% continuation in the last iteration of  run_contins. This is indeed the
% case. Thus in run_contins we  also  need   to  look  at  the number of
% continuations to determine whether we can   stop.  However we had some
% questions: we might need to look  whether   the  new continuation is a
% variant of a continuation that is   already present (to avoid infinite
% loops).
%
% j(a,b).
% j(X,Y) :- j(_,_), j(Y,X).

expected_variants([j(_,_)]).
expected_answers_for_variant(j(_,_),[j(a,b),j(b,a)]).
j_expected_answers([a-b,b-a]).

:- table j/2.

j(a,b) :-
	feedback('j 1: give answer'). % TODO: check why we don*t have to save this answer here explicitly.
j(X,Y) :-
	feedback('j 2: before'),
	j(_,_),
	feedback('j 2: between'),
	j(Y,X),
	feedback('j 2: after').

test(ex8) :-
	compare_real_expected_answers(j,2,j_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex8).


		 /*******************************
		 *           EXAMPLE 9A		*
		 *******************************/

:- begin_tests(tabling_ex9a, [cleanup(abolish_all_tables)]).
% The reachability predicate can be written in several variants.
% Example 1: a
% p(X,Y) :- p(X,Z), p(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9a:
% p(X,Y) :- p(X,Z), e(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9b:
% p(X,Y) :- e(X,Z), p(Z,Y).
% p(X,Y) :- e(X,Y).
%
% Example 9c:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- p(X,Z), p(Z,Y).
%
% Example 9d:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- p(X,Z), e(Z,Y).
%
% Example 9e:
% p(X,Y) :- e(X,Y).
% p(X,Y) :- e(X,Z), p(Z,Y).

expected_variants([a(_,_)]).
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).

% The answers we expect for reachability   variants,  returned as a list
% with entries of the form X-Y. The order does not matter.

reachability_expected_answers([1-2,2-3,1-3]).

:- table a/2.

a(X,Y) :-
	feedback('before'),
	a(X,Z),
	feedback('between: prove ~w~n',[e(Z,Y)]),
	e(Z,Y).
a(X,Y) :-
	e(X,Y).

% Test facts for examples 1 and 2.
e(1,2).
e(2,3).

test(ex9a) :-
	compare_real_expected_answers(a,2,reachability_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.
:- end_tests(tabling_ex9a).


		 /*******************************
		 *           EXAMPLE 9B		*
		 *******************************/

:- begin_tests(tabling_ex9b, [cleanup(abolish_all_tables)]).
expected_variants([a(3,_),a(2,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(3,_),[]).
expected_answers_for_variant(a(2,_),[a(2,3)]).
reachability_expected_answers([1-2,2-3,1-3]).

:- table a/2.

a(X,Y) :-
	feedback('before'),
	e(X,Z),
	feedback('between: prove ~w~n',[a(Z,Y)]),
	a(Z,Y).
a(X,Y) :-
	e(X,Y).

e(1,2).
e(2,3).

test(ex9b) :-
	compare_real_expected_answers(a,2,reachability_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex9b).



		 /*******************************
		 *           EXAMPLE 9C		*
		 *******************************/

:- begin_tests(tabling_ex9c, [cleanup(abolish_all_tables)]).
expected_variants([a(2,_),a(3,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(2,_),[a(2,3)]).
expected_answers_for_variant(a(3,_),[]).

reachability_expected_answers([1-2,2-3,1-3]).

:- table a/2.

a(X,Y) :- e(X,Y).
a(X,Y) :- feedback('before'), a(X,Z), feedback('between'),  a(Z,Y).

e(1,2).
e(2,3).

test(ex9c) :-
	compare_real_expected_answers(a,2,reachability_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex9c).


		 /*******************************
		 *           EXAMPLE 9D		*
		 *******************************/

:- begin_tests(tabling_ex9d, [cleanup(abolish_all_tables)]).
expected_variants([a(_,_)]).
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).

reachability_expected_answers([1-2,2-3,1-3]).

:- table a/2.

a(X,Y) :- e(X,Y).
a(X,Y) :- feedback('before'), a(X,Z), feedback('between'),  e(Z,Y).

e(1,2).
e(2,3).

test(ex9d) :-
	compare_real_expected_answers(a,2,reachability_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex9d).


		 /*******************************
		 *           EXAMPLE 9E		*
		 *******************************/

:- begin_tests(tabling_ex9e, [cleanup(abolish_all_tables)]).
expected_variants([a(3,_),a(2,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(3,_),[]).
expected_answers_for_variant(a(2,_),[a(2,3)]).

reachability_expected_answers([1-2,2-3,1-3]).

:- table a/2.

a(X,Y) :- e(X,Y).
a(X,Y) :- feedback('before'), e(X,Z), feedback('between'),  a(Z,Y).

e(1,2).
e(2,3).

test(ex9d) :-
	compare_real_expected_answers(a,2,reachability_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex9e).


		 /*******************************
		 *           EXAMPLE 10		*
		 *******************************/

:- begin_tests(tabling_ex10, [cleanup(abolish_all_tables)]).
%d(X) <- e(Y), Y < 5, X is Y + 1.  % Will never run, because e doesn't have any facts
%d(X) <- d(Y), Y < 20, X is Y + 5. % Should run
%d(0).
%e(X) <- d(Y), Y < 5, X is Y + 1.
%% No facts for e

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0),d(2),d(4),d(5),d(7),d(9),
				   d(10),d(12),d(14),d(15),d(17),d(19),
				   d(20),d(22),d(24)]).
expected_answers_for_variant(e(_),[e(5),e(3),e(1)]).

d_expected_answers([0,2,4,5,7,9,10,12,14,15,17,19,20,22,24]).
e_expected_answers([5,3,1]).

:- table d/1, e/1.

d(X) :- e(Y), Y < 5, X is Y + 1.  % Will never run, because e doesn't have any facts (initially)
d(X) :- d(Y), Y < 20, X is Y + 5. % Should run
d(0).
e(X) :- d(Y), Y < 5, X is Y + 1.
% No facts for e

test(ex10) :-
	compare_real_expected_answers(d,1,d_expected_answers),
	compare_real_expected_answers(e,1,e_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex10).


		 /*******************************
		 *           EXAMPLE 11		*
		 *******************************/

:- begin_tests(tabling_ex11, [cleanup(abolish_all_tables)]).
% Expected answers:
% d(0), d(2), d(4)
% e(1), e(3), e(5)

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0),d(2),d(4)]).
expected_answers_for_variant(e(_),[e(1),e(3),e(5)]).

d_expected_answers([0,2,4]).
e_expected_answers([1,3,5]).

:- table d/1, e/1.

d(X) :- e(Y), Y < 5, X is Y + 1.
d(0).
e(X) :- d(Y), Y < 5, X is Y + 1.
% No facts for e

test(ex11) :-
	compare_real_expected_answers(d,1,d_expected_answers),
	compare_real_expected_answers(e,1,e_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex11).


		 /*******************************
		 *           EXAMPLE 12		*
		 *******************************/

:- begin_tests(tabling_ex12, [cleanup(abolish_all_tables)]).
expected_variants([f(4),f(3),f(2),f(0),f(1),e(_),d(_),f(_)]).
% Note: f(4) and f(0) are empty tables, but they are there...
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,4,X),L).
expected_answers_for_variant(f(4),[]).
expected_answers_for_variant(f(3),[f(3)]).
expected_answers_for_variant(f(2),[f(2)]).
expected_answers_for_variant(f(0),[]).
expected_answers_for_variant(f(1),[f(1)]).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,3,X),L).
expected_answers_for_variant(f(_),L) :-	% JW: Added
  findall(f(X),between(1,3,X),L).

d_expected_answers([0,1,2,3,4]).
e_expected_answers([0,1,2,3]).
f_expected_answers([1,2,3]).

:- table d/1, e/1, f/1.

% Something like:
d(X) :- e(Y), feedback('d_aux: after e(Y): ~w\n',[e(Y)]), X is Y + 1, X < 5.
d(0).
% Number of predicates involved in mutual recursion will increase at runtime
e(X) :- d(X), f(X),
	feedback('e_aux: at end of clause; head is now ~w\n',[e_aux(X)]).
e(0) :- feedback('using fact e(0)\n',[]).
f(X) :- e(Y), X is Y + 1, X < 4.

test(ex12) :-
	compare_real_expected_answers(d,1,d_expected_answers),
	compare_real_expected_answers(e,1,e_expected_answers),
	compare_real_expected_answers(f,1,f_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex12).

		 /*******************************
		 *           EXAMPLE 13		*
		 *******************************/

:- begin_tests(tabling_ex13, [cleanup(abolish_all_tables)]).
expected_variants([p(2,_),p(1,_),p(_,_)]).
expected_answers_for_variant(p(_,_),L) :-
  findall(p(X,Y),(between(1,2,X),between(1,2,Y)),L).
expected_answers_for_variant(p(2,_),L) :-
  findall(p(2,X),between(1,2,X),L).
expected_answers_for_variant(p(1,_),L) :-
  findall(p(1,X),between(1,2,X),L).

% The answers we expect for example 13, returned as a list with entries of the form X-Y. The order does not matter.
% We expect four answers.
p_expected_answers([1-2,2-1,1-1,2-2]).

:- table p/2.

p(X,Y) :-
	feedback('before'), p(X,Z), feedback('between'), p(Z,Y).
p(X,Y) :- e2(X,Y).

e2(1,2).
e2(2,1).

test(ex12) :-
	compare_real_expected_answers(p,2,p_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex13).


		 /*******************************
		 *           EXAMPLE 14		*
		 *******************************/

:- begin_tests(tabling_ex14, [cleanup(abolish_all_tables)]).
% Simpler example than example12.pl, but the number of predicates involved in mutual recursion will also increase at runtime.

expected_variants([p(3,_),p(2,_),q(2,_),q(3,_),p(_,_),q(_,_)]).
% Note: p(3,_) and q(3,_) are empty tables, but they are there.
expected_answers_for_variant(p(_,_),[p(1,2),p(2,3),p(1,3)]).
expected_answers_for_variant(p(3,_),[]).
expected_answers_for_variant(p(2,_),[p(2,3)]).
expected_answers_for_variant(q(_,_),[q(1,2),q(1,3),q(2,3)]).
expected_answers_for_variant(q(2,_),[q(2,3)]).
expected_answers_for_variant(q(3,_),[]).

p_expected_answers([1-2,2-3,1-3]).
q_expected_answers([1-2,1-3,2-3]).	% JW: added

go :-
  once(p(_X,_Y)).

:- table p/2, q/2.

p(X,Y) :- p(X,Z), q(Z,Y).
p(X,Y) :- e(X,Y).
q(X,Y) :- p(X,Y).

e(1,2).
e(2,3).

test(ex14) :-
	compare_real_expected_answers(p,2,p_expected_answers),
	compare_real_expected_answers(q,2,q_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex14).


		 /*******************************
		 *           EXAMPLE 15		*
		 *******************************/

:- begin_tests(tabling_ex15, [cleanup(abolish_all_tables)]).
% Example designed to test whether the true   in fresh status works fine
% in case only a continuation is saved. See issue 55.

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).

d_expected_answers([0,1,2,3,4,5]).
e_expected_answers([0,1,2,3,4,5]).

:- table d/1, e/1.

d(X) :- e(Y), Y < 5, X is Y + 1.
d(0).
e(X) :- d(Y), e(_), Y < 5, X is Y + 1.
e(0).

test(ex15) :-
	compare_real_expected_answers(d,1,d_expected_answers),
	compare_real_expected_answers(e,1,e_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex15).


		 /*******************************
		 *           EXAMPLE 16		*
		 *******************************/

:- begin_tests(tabling_ex16, [cleanup(abolish_all_tables)]).
% Expected outcome: there should not be a crash because d was already complete

expected_variants([d(_),e(_),f(_),g(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).
expected_answers_for_variant(f(_),L) :-
  findall(f(X),between(0,5,X),L).
expected_answers_for_variant(g(_),L) :-
  findall(g(X),between(0,5,X),L).

% Two mutually recursive predicates:
% d(X) :- e(Y), Y < 5, X is Y + 1.
% d(0).
%
% e(X) :- d(Y), Y < 5, X is Y + 1.
% e(0).

:- table d/1, e/1, f/1, g/1.

d(X) :-
	feedback('d_aux: before calling e(Y)'),
	e(Y),
	feedback('d_aux: after calling e(Y)'),
	feedback('d_aux: Y is ~w~n',[Y]),
	Y < 5,
	feedback('d_aux: Y < 5 OK\n',[]),
	(   X is Y + 1
	->  feedback('d_aux: is OK\n',[])
	;   feedback('d_aux: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('d_aux: X is ~w~n',[X]).
d(0).

e(X) :-
	feedback('e_aux: before calling d(Y)'),
	d(Y),
	feedback('e_aux: after calling d(Y)'),
	feedback('e_aux: Y is ~w~n',[Y]),
	Y < 5,
	feedback('e_aux: Y < 5 OK\n', []),
	(   X is Y + 1
	->  feedback('e_aux: is OK\n',[])
	;   feedback('e_aux: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('e_aux: X is ~w~n',[X]).
e(0).

f(X) :- g(Y), Y < 5, X is Y + 1.
f(0).

g(X) :- f(Y), Y < 5, X is Y + 1.
g(0).

test(ex16) :-
	once((d(_X), f(_Y))).

:- end_tests(tabling_ex16).


		 /*******************************
		 *           EXAMPLE 17		*
		 *******************************/

:- begin_tests(tabling_ex17, [cleanup(abolish_all_tables)]).
% Smaller version of example 10.

% Expected answers

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0), d(2), d(5)]).
expected_answers_for_variant(e(_),[e(1)]).

:- table d/1, e/1.

d(X) :- e(Y), Y < 2, X is Y + 1.  % Will never run, because e doesn't have any facts (initially)
d(X) :- d(Y), Y < 2 , X is Y + 5. % Should run
d(0).
e(X) :- d(Y), Y < 2, X is Y + 1.
% No facts for e

test(ex17) :-
	once(d(_)),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex17).


:- begin_tests(tabling_clpdf, [cleanup(abolish_all_tables)]).

:- use_module(library(tabling)).
:- table fib/2.
:- use_module(library(clpfd)).

fib(1, 1).
fib(2, 2).
fib(N, X) :-
    N #> 2, N1 #= N-1, N2 #= N-2,
    fib(N1, X1),
    fib(N2, X2),
    X #= X1+X2.

test(fib_error, error(type_error(free_of_attvar, _))) :-
	fib(_N, 13).

:- end_tests(tabling_clpdf).


		 /*******************************
		 *     MODE DIRECTED TABLING	*
		 *******************************/

:- begin_tests(tabling_eruption, [cleanup(abolish_all_tables)]).

:- table
  eruption(lattice(prob_sum_e/3)),
  sudden_energy_release/0,
  fault_rupture(_,lattice(prob_sum_e/3)).

eruption(P)  :-
  sudden_energy_release,
  fault_rupture(_,P0),
  P is P0*0.6.
% If there is a sudden energy release under the island and there is a fault
% rupture, then there can be an eruption of the volcano on the island with
% probability 0.6

sudden_energy_release.
% The energy release occurs with certainty

fault_rupture(southwest_northeast,0.7).
fault_rupture(east_west,0.6).
% we are sure that ruptures occur

prob_sum_e(A,B,C):-
  C is 1-(1-A)*(1-B).

test(tabling_eruption, P =:= 0.6288) :-
	eruption(P).

:- end_tests(tabling_eruption).

:- begin_tests(tabling_sneezing, [cleanup(abolish_all_tables)]).

:- use_module(library(tabling)).
:- table
    sneezing(_,lattice(prob_sum)),
    flu(_,lattice(prob_sum)),
    hay_fever(_,lattice(prob_sum)).

sneezing(X,P) :- flu(X,P0), P is P0*0.3.
% if X has the flu, there is a probability of 0.3 that he sneezes

sneezing(X,P) :- hay_fever(X,P0), P is P0*0.2.
% if X has hay fever, there is a probability of 0.2 that he sneezes

flu(bob,0.2).
% bob has the flu with prob 0.2

hay_fever(bob,0.4).
% bob has hay fever with prob 0.4

prob_sum(A,B,C):-
  C is 1-(1-A)*(1-B).

test(tabling_sneezing, P =:= 0.1352000000000001) :-
	sneezing(bob, P).

:- end_tests(tabling_sneezing).

:- begin_tests(tabling_yappath, [cleanup(abolish_all_tables)]).
:- use_module(library(tabling)).

:- table
    path(index, index, first).

path(X, Y, N) :- path(X, Z, N1), edge(Z, Y), N is N1 + 1.
path(X, Y, 1) :- edge(X, Y).

edge(a, b).
edge(b, a).

min(A,B,C) :-
    writeln(A-B-C),
    C is min(A,B).

test(yappath, set(t(X,Y,L) == [t(a,a,2),t(a,b,1),t(b,a,1),t(b,b,2)])) :-
    path(X,Y,L).

:- end_tests(tabling_yappath).

:- begin_tests(tabling_minpath, [cleanup(abolish_all_tables)]).
:- use_module(library(tabling)).
:- table connection(_,_,min).

connection(X, Y,1) :-
        connection(X, Y).
connection(X, Y,N) :-
        connection(X, Z,N1),
        connection(Z, Y),
        N is N1+1.

connection('Amsterdam', 'Schiphol').
connection('Amsterdam', 'Haarlem').
connection('Schiphol', 'Leiden').
connection('Haarlem', 'Leiden').
connection('Amsterdam', 'Leiden').

test(tabling_minpath, N =:= 1) :-
	connection('Amsterdam','Leiden',N).

:- end_tests(tabling_minpath).

:- begin_tests(tabling_train, [cleanup(abolish_all_tables)]).
:- use_module(library(tabling)).
:- table train(_,_,lattice(shortest/3)).

train(X, Y, [X,Y]) :-
    train(X, Y).
train(X, Y, P) :-
    train(X, Z, P0),
    train(Z, Y),
    append(P0, [Y], P).

train('Amsterdam', 'Schiphol').
train('Amsterdam', 'Haarlem').
train('Schiphol',  'Leiden').
train('Haarlem',   'Leiden').
train('Amsterdam', 'Leiden').

shortest(P1, P2, P):-
    length(P1, L1),
    length(P2, L2),
    (   L1 < L2
    ->  P = P1
    ;   P = P2
    ).

test(tabling_train, P == ['Amsterdam','Leiden']) :-
	train('Amsterdam','Leiden', P).

:- end_tests(tabling_train).

:- begin_tests(moded_tabling_path, [cleanup(abolish_all_tables)]).
:- use_module(library(tabling)).
:- table
    path(_,_,lattice(or/3)),
    edge(_,_,lattice(or/3)).

path(X,X,one).
path(X,Y,C):-
    edge(X,Z,A),
    path(Z,Y,B),
    and(A,B,C).

path(A,B,zero) :-
    nonvar(A),
    nonvar(B).

edge(a,b,e(a,b)).
edge(b,e,e(b,e)).
edge(a,e,e(a,e)).

edge(A,B,zero):-
    nonvar(A),
    nonvar(B).

or(zero, B, B) :- !.
or(B, zero, B) :- !.
or(one, one, one) :- !.
or(A,A,A):-!.
or(A, B, or(A,B)).

and(zero, _, _) :- !, fail.
and(_, zero, _) :- !, fail.
and(one, B, B) :- !.
and(B, one, B) :- !.
and(A,A,A):-!.
and(A, B, and(A,B)).

test(path, T == or(e(a, e), and(e(a, b), e(b, e)))) :-
	path(a, e, T),
	assertion(ground(T)),
	assertion(ok_path(T)).

ok_path(or(e(a, e), and(e(a, b), e(b, e)))).
ok_path(or(and(e(a, b), e(b, e)), e(a, e))).

:- end_tests(moded_tabling_path).



		 /*******************************
		 *	      COMMON		*
		 *******************************/

before    :- debug(tabling, 'before',  []).
before(I) :- debug(tabling, 'before ~w',  [I]).
between   :- debug(tabling, 'between', []).
feedback(Fmt) :- debug(tabling, Fmt, []).
feedback(Fmt,Args) :- debug(tabling, Fmt, Args).
end       :- debug(tabling, 'end',   []).
end(I)    :- debug(tabling, 'end ~w',   [I]).
