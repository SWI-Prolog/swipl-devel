/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemak@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

:- module(test_misc, [test_misc/0]).
:- use_module(library(plunit)).

/** <module> Misc tests

Tests that are hard to classify

@author	Jan Wielemaker
*/

test_misc :-
	run_tests([ misc
		  ]).

:- begin_tests(misc).

p(_).
p(C) :- prolog_cut_to(C).

test(read_only_flag, Access == read) :-
	'$current_prolog_flag'(arch, _, _Global, Access, _Type).
test(cut_to, all(X == [1])) :-
	prolog_current_choice(Ch),
	between(1, 5, X),
	prolog_cut_to(Ch).
test(cut_to, error(existence_error(choice,_))) :-
	(   prolog_current_choice(C),
	    p(C)
	*-> fail
	;   fail
	).

:- end_tests(misc).
