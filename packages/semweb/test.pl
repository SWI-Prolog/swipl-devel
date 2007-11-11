/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2007, University of Amsterdam

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

:- asserta(file_search_path(foreign, '../sgml')).
:- asserta(file_search_path(foreign, '../clib')).
:- asserta(file_search_path(foreign, '../zlib')).
:- asserta(file_search_path(library, '../sgml')).
:- asserta(file_search_path(library, '../clib')).
:- asserta(file_search_path(library, '../zlib')).
:- asserta(file_search_path(library, '../sgml/RDF')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '..')).
:- asserta(user:file_search_path(foreign, '.')).

:- use_module(library(plunit)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_zlib_plugin)).
:- use_module(library(semweb/rdf_http_plugin)).


:- begin_tests(load,
	       [ setup(rdf_reset_db),
		 cleanup(rdf_reset_db)
	       ]).

test(file, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('Tests/test-001.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(file, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('file://Tests/test-001.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(gzip_file, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('Tests/test-002.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(gzip_file, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('file://Tests/test-002.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(http, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('http://gollem.science.uva.nl/SWI-Prolog/Tests/semweb/test-001.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(gzip_http, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('http://gollem.science.uva.nl/SWI-Prolog/Tests/semweb/test-002.rdf', [silent(true)]),
	rdf_statistics(triples(N)).

test(gzip_http, [true(N == 1), cleanup(rdf_reset_db)]) :-
	rdf_load('http://gollem.science.uva.nl/SWI-Prolog/Tests/semweb/test-002.rdf.gz', [silent(true)]),
	rdf_statistics(triples(N)).

:- end_tests(load).

:- begin_tests(inverse).

test(set,  [cleanup(rdf_reset_db)]) :-
	rdf_assert(r1, p1, r2),
	rdf_set_predicate(p2, inverse_of(p1)),
	rdf_has(r2, p2, r1).
test(clear,  [cleanup(rdf_reset_db)]) :-
	rdf_assert(r1, p1, r2),
	rdf_set_predicate(p2, inverse_of(p1)),
	rdf_has(r2, p2, r1),
	rdf_set_predicate(p2, inverse_of([])),
	\+ rdf_has(r2, p2, r1).

:- end_tests(inverse).
