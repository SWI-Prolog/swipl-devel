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

:- module(test_json,
	  [ test_json/0
	  ]).
:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(foreign, '../clib')).
:- asserta(user:file_search_path(foreign, '../sgml')).
:- asserta(user:file_search_path(library, '../plunit')).
:- asserta(user:file_search_path(library, '../clib')).
:- asserta(user:file_search_path(library, '../sgml')).

:- use_module(library(plunit)).
:- use_module(json).

test_json :-
	run_tests([ json_read,
		    json_convert,
		    json_http
		  ]).

:- begin_tests(json_read).

test(true, X == @(true)) :-
	atom_json_term(true, X, []).
test(true, X == true) :-
	atom_json_term(true, X, [true(true)]).

test(string, X == hello) :-
	atom_json_term('"hello"', X, []).
test(string, X == '\\\b\f\n\r\t') :-
	atom_json_term('"\\\\\\b\\f\\n\\r\\t"', X, []).
test(string, X == '\u1234') :-
	atom_json_term('"\\u1234"', X, []).

test(int, X == 42) :-
	atom_json_term('42', X, []).
test(int, X == -42) :-
	atom_json_term('-42', X, []).

test(float, X == 3.14) :-
	atom_json_term('3.14', X, []).
test(float, X == -3.14) :-
	atom_json_term('-3.14', X, []).
test(float, X == 1000.0) :-
	atom_json_term('1e3', X, []).
test(float, X == 1000.0) :-
	atom_json_term('1.0e3', X, []).
test(float, X == 0.001) :-
	atom_json_term('1e-3', X, []).
test(float, X == 0.001) :-
	atom_json_term('1.0e-3', X, []).

test(empty, X == json([])) :-
	atom_json_term({}, X, []).
test(empty, X == json([])) :-
	atom_json_term('  {  } ', X, []).
test(empty, X == json([])) :-
	atom_json_term('  {\n//comment\n} ', X, []).


:- end_tests(json_read).


		 /*******************************
		 *	      CONVERT		*
		 *******************************/

:- begin_tests(json_convert).

:- use_module(json_convert).

:- json_object
	point(x:integer, y:integer),
	tpoint(x:integer, y:integer)+[type=point],
	fpoint(x:float, y:float).

test(pt2json, JSON == json([x=25,y=50])) :-
	prolog_to_json(point(25,50), JSON).
test(pt2json, JSON == json([x=25,y=50,type=point])) :-
	prolog_to_json(tpoint(25,50), JSON).

test(json2pt, X == point(25,50)) :-
	json_to_prolog(json([x=25,y=50]), X).
test(json2pt, X == point(25,50)) :-
	json_to_prolog(json([y=50,x=25]), X).
test(json2pt, X == fpoint(25.1,50.0)) :-
	json_to_prolog(json([y=50.0,x=25.1]), X).
test(json2pt, T == T2) :-
	T = json([y=50,x=25.1]),
	json_to_prolog(json([y=50,x=25.1]), T2).
test(json2pt, X == tpoint(25,50)) :-
	json_to_prolog(json([x=25,y=50,type=point]), X).

:- end_tests(json_convert).


		 /*******************************
		 *	       HTTP		*
		 *******************************/

:- use_module(http_json).
:- use_module(http_client).
:- use_module(thread_httpd).

:- dynamic
	port/1.

make_server :-
	retractall(port(_)),
	http_server(reply,
		    [ port(Port),
		      workers(1)
		    ]),
	assert(port(Port)).

kill_server :-
	retract(port(Port)),
	http_stop_server(Port, []).

reply(Request) :-
	memberchk(path('/json/echo'), Request), !,
	http_read_json(Request, JSON),
	reply_json(JSON).

echo(Term, Reply) :-
	port(Port),
	format(string(URL), 'http://localhost:~w/json/echo', [Port]),
	http_post(URL, json(Term), Reply, []).

:- begin_tests(json_http, [ setup(make_server),
			    cleanup(kill_server)
			  ]).

test(echo, X == 42) :-
	echo(42, X).
test(echo, X == -3.14) :-
	echo(-3.14, X).
test(echo, X == name) :-
	echo(name, X).
test(echo, X == [1,2,3]) :-
	echo([1,2,3], X).
test(echo, X == json([name=json, arity=2])) :-
	echo(json([name=json, arity=2]), X).

test(unicode, X == Atom) :-
	Atom = '\u0411\u0435\u0437\u0443\u043f\u0440\u0435\u0447\u043d\u043e\u0435',
	echo(Atom, X).
test(quote, X == Atom) :-
	Atom = 'hello, "world"',
	echo(Atom, X).
test(control, X == Atom) :-
	Atom = 'hello\n\t\r\b\003\',
	echo(Atom, X).

:- end_tests(json_http).

:- multifile
	user:message_hook/3.

user:message_hook(httpd_stopped_worker(_, true), _Kind, _Lines).
