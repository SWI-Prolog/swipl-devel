#!/usr/bin/pl -q -g main -s
/*  $Id$

    Part of SWI-Prolog

    This example code is in the public domain
*/

:- use_module(library(cgi)).

main :-
	cgi_get_form(Arguments),
	format('Content-type: text/html~n~n', []),
	format('<html>~n', []),
	format('<head>~n', []),
	format('<title>Simple SWI-Prolog CGI script output</title>~n', []),
	format('</head>~n~n', []),
	format('<body>~n', []),
	format('<h1>Form arguments</h1>'),
	format('<p>', []),
	print_args(Arguments),
	format('<body>~n</html>~n', []),
	halt.

print_args([]).
print_args([A0|T]) :-
	A0 =.. [Name, Value],
	format('<b>~w</b>=<em>~w</em><br>~n', [Name, Value]),
	print_args(T).
