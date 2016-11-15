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

:- module(test_env, [test_env/0]).
:- use_module(library(plunit)).

/** <module> Text environment handling

@author	Jan Wielemaker
*/

test_env :-
	run_tests([ env
		  ]).

:- begin_tests(env).

test(long, Got == Set) :-
	length(Chars, 1100),
	maplist(=(.), Chars),
	atom_chars(Set, Chars),
	setenv(tv, Set),
	getenv(tv, Got).

:- end_tests(env).
