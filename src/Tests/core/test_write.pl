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

:- module(test_write, [test_write/0]).
:- use_module(library(plunit)).

/** <module> Misc tests

Tests that are hard to classify

@author	Jan Wielemaker
*/

test_write :-
	run_tests([ portray,
		    write_canonical
		  ]).

:- begin_tests(portray).

user:portray(ptray_test) :-
	throw(nono).

test(exception, throws(nono)) :-
	print(ptray_test).
test(blob, Text == text) :-
	open_null_stream(S),
	with_output_to(atom(Text),
		       write_term(S, [ blobs(portray),
				       portray_goal(portray_stream)
				     ])),
	close(S).

portray_stream(S, _) :-
	stream_property(S, type(T)),
	format('~w', [T]).

:- end_tests(portray).

:- begin_tests(write_canonical).

test(numbervars, X = 'x(_,_)') :-
	with_output_to(atom(X),
		       write_canonical(x(_,_))).
test(numbervars, X = 'x(A,A)') :-
	with_output_to(atom(X),
		       write_canonical(x(B,B))).
test(numbervars, X = 'x(\'$VAR\'(1),_)') :-
	with_output_to(atom(X),
		       write_canonical(x('$VAR'(1),_))).

:- end_tests(write_canonical).
