/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2009, VU University, Amsterdam

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

:- module(test_cgi, [test_cgi/0]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(cgi).
:- use_module(uri).

test_cgi :-
	run_tests([ cgi
		  ]).

trip(FormIn, FormOut) :-
	uri_query_components(String, FormIn),
	setenv('QUERY_STRING', String),
	cgi_get_form(FormOut).

n_list_of(0, _, []) :- !.
n_list_of(I, H, [H|T]) :-
	I2 is I - 1,
	n_list_of(I2, H, T).

:- begin_tests(cgi).

test(atom, In == Out) :-
	In = [name(value)],
	trip(In, Out).
test(atom, In == Out) :-
	numlist(32, 126, Chars),
	n_list_of(10, Chars, ListOfStrings),
	append(ListOfStrings, AllChars),
	atom_codes(Value, AllChars),
	In = [name(Value)],
	trip(In, Out).
test(unicode, In == Out) :-
	numlist(32, 1100, Chars),
	atom_codes(Value, Chars),
	In = [name(Value)],
	trip(In, Out).
test(integer, In == Out) :-
	In = [age(2394395340490340)],
	trip(In, Out).
test(float, In == Out) :-
	In = [age(42.0)],
	trip(In, Out).

:- end_tests(cgi).

