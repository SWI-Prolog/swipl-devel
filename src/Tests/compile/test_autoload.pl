/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2020, VU University Amsterdam

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

:- module(test_autoload,
          [ test_autoload/0
          ]).
:- use_module(library(plunit)).
:- use_module(library(debug)).

/** <module> Test autoloading
*/

test_autoload :-
    run_tests([ autoload
              ]).


:- begin_tests(autoload).

% test that we can define a predicate in  user, declare it autoload in a
% module and get the definition from the autoload location.

user:day_of_the_year(_, not_an_integer).
:- autoload(library(date), [day_of_the_year/2]).

test(not_from_user) :-
    day_of_the_year(date(1952, 03, 11), Day),
    assertion(integer(Day)).

:- end_tests(autoload).
