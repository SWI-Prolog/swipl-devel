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


:- module(test_prolog_clause,
	  [ test_prolog_clause/0
	  ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(prolog_clause)).

/** <module> Test source-information on clauses

This  module  tests  clause_info/4,  a  vital  part  of  the  SWI-Prolog
source-level debugger.
*/

%t :-
%	debug(clause_info),
%	test_prolog_clause.

test_prolog_clause :-
	run_tests([ prolog_clause
		  ]).

:- begin_tests(prolog_clause).

t0 :-	a(X), b(Y), X=Y.
t1 :-	a, _ = hello, b.
t2 :-	a(X), a(b) = X, b(X).
t3 :-	a, _ == hello, b.
t4 :-	a(X), x == X, b(X).
t5 :-	i(A), B is A-1, b(B).

a.
b.
a(_).
b(_).
i(10).

test_ci(Head) :-
	clause(Head, _Body, Ref),
	clause_info(Ref, File, _TermPos, _NameOffset),
	atom(File).

test(t0) :- test_ci(t0).
test(t1) :- test_ci(t1).
test(t2) :- test_ci(t2).
test(t3) :- test_ci(t3).
test(t4) :- test_ci(t4).

:- end_tests(prolog_clause).
