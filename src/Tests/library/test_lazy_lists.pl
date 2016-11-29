/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2016, University of Amsterdam
			 CWI, Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(test_lazy_lists,
	  [ test_lazy_lists/0
	  ]).
:- use_module(library(lazy_lists)).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).

test_lazy_lists :-
	run_tests([ lazy_lists
		  ]).

:- begin_tests(lazy_lists).

test(list, Rest == `world`) :-
	setup_call_cleanup(
	    ( open_string("hello world", In),
	      lazy_list(lazy_get_codes(In, 1), List)
	    ),
	    phrase(("hello", " ", string(Rest)), List),
	    close(In)),
	!.

:- end_tests(lazy_lists).
