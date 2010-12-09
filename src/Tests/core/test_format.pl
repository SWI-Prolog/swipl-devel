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

:- module(test_format, [test_format/0]).
:- use_module(library(plunit)).

/** <module> Test Prolog text formatting primitives

This module is a Unit test for  Prolog format/2, etc.

@author	Jan Wielemaker
*/

test_format :-
	run_tests([ format
		  ]).

:- begin_tests(format).

test(fail, fail) :-
	format('~@', [fail]).
test(fail, throws(error(42))) :-
	format('~@', [throw(error(42))]).
test(no_stream, error(existence_error(stream, S))) :-
	S = stream_dhfuweiu,
	format(S, fmt, []).

:- end_tests(format).
