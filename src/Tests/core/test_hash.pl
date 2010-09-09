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
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/


:- module(test_hash,
	  [ test_hash/0
	  ]).
:- use_module(library(plunit)).

test_hash :-
	run_tests([ variant_sha1
		  ]).

:- begin_tests(variant_sha1).

test(atom) :-
	variant_sha1(this_is_an_atom, Hash),
	atom_length(Hash, 40).
test(vars, Hash1 == Hash2) :-
	v(A), v(B),
	variant_sha1(x(A), Hash1),
	variant_sha1(x(B), Hash2).
test(shared, Hash1 == Hash2) :-
	A = x(C),
	variant_sha1(x(A,A), Hash1),
	variant_sha1(x(x(C),x(C)), Hash2).
					% error handling
test(cycle, [sto(rational_trees),error(type_error(acyclic_term, A))]) :-
	A = a(A),
	variant_sha1(A, _).
test(cycle, [sto(rational_trees),error(type_error(acyclic_term, _))]) :-
	A = a(A),
	variant_sha1(x(A), _).
test(attvar, error(_)) :-
	dif(X, 3),
	variant_sha1(X, _).
test(attvar, true) :-
	dif(X, 3), % error(_) fails because subsumes_term does not deal with attvar
	catch(variant_sha1(x(a(X)), _), _, true).

v(_).

:- end_tests(variant_sha1).
