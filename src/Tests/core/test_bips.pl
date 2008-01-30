/*  $Id$

    Part of SWI-Prolog

    Author:        Ulrich Neumerkel
    WWW:           http://www.swi-prolog.org
    Copyright (C): Ulrich Neumerkel

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/** <module> Test built-in predicates

This module is a test-frame for testing built-in predicates.

*/

:- module(test_bips, [test_bips/0]).
:- use_module(library(plunit)).

test_bips :-
	run_tests([bips,bips_occurs_check_error]).

has_occurs_check_flag :-
	catch(current_prolog_flag(occurs_check, _), _, fail).


:- begin_tests(bips).

/* draft examples: */
test(iso_8_18_2_4,[Length==3]) :-
	length([1, 2, 3], Length).
test(iso_8_18_2_4,[List =@= [_,_,_]]) :-
	length(List, 3).
test(iso_8_18_2_4,[fail]) :- % maybe disagreement error(domain_error(not_less_than_zero, -2))
	length(_List, -2).
test(iso_8_18_2_4,[all(List-Length =@= [[]-0, [_]-1, [_,_]-2])]) :-
	length(List,Length),
	( Length >= 2 -> ! ; true ).
/* addendum in (probable) agreement */
test(iso_8_18_2_3,[error(type_error(integer,a))]) :-
	length(_, a).
test(iso_8_18_2_3,[sto(rational_trees),Length==3]) :-
	List = [List,List,List],
	length(List,Length).
test(iso_8_18_2_3,[error(type_error(integer,(1+2)))]) :-
	length(_,1+2).

/* (current) disagreement with draft */
test(noniso, [error(type_error(list,a))]) :-
	length(a,_).
test(noniso, [error(type_error(list,_))]) :-
	length([_,_|a],_).
test(noniso, [sto(rational_trees),error(type_error(list,List))]) :-
	List = [List,List,List|List],
	length(List, _).

test(swi, [fail, condition(current_prolog_flag(bounded, false))]) :- % Item#285
	length(_,-300000000000000000).

:- end_tests(bips).

:- begin_tests(bips_occurs_check_error,[condition(has_occurs_check_flag)]).

error_unification :-
	current_prolog_flag(occurs_check,error).

/* Item#310  PL_unify and occurs check error */
test(term_variable, [condition(error_unification),error(occurs_check(_, _))]) :-
	term_variables(GVars,GVars).
test(term_variable, [condition(error_unification),error(occurs_check(_, _))]) :-
	X = s(_),
	arg(1,X,X).
test(term_variable, [condition(error_unification),error(occurs_check(_, _))]) :-
	X =.. [s,X].
test(term_variable, [condition(error_unification),error(occurs_check(_, _))]) :-
 	copy_term(X-X,Y-{Y}).
test(findall, [condition(error_unification),error(occurs_check(_,_))]) :-
	findall(X-X,true,[{X}-X]).
%test(clause, [condition(error_unification),error(occurs_check(_,_))]) :-
%	clause(equal(A,+A), _).
test(atom_to_term, [condition(error_unification),error(occurs_check(_,_))]) :-
	atom_to_term('X-X',X-{X},_).
test(sort,[condition(error_unification),error(occurs_check(_,_))]) :-
	sort([X,+X],[Y,Y]).

:- end_tests(bips_occurs_check_error).


