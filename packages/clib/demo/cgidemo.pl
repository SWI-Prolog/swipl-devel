/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker

    Copyright (C) 1999 SWI, University of Amseterdam. All rights reserved.
*/

:- use_module(library(cgi)).

main :-
	get_form(Arguments),
	format('Content-type: text/html~n~n', []),
	format('<HTML>~n', []),
	format('<HEAD>~n', []),
	format('<TITLE>Simple SWI-Prolog CGI script output</TITLE>~n', []),
	format('</HEAD>~n~n', []),
	format('<BODY>~n', []),
	format('<P>', []),
	print_args(Arguments),
	format('<BODY>~n</HTML>~n', []).

print_args([]).
print_args([A0|T]) :-
	A0 =.. [Name, Value],
	format('<B>~w</B>=<EM>~w</EM><BR>~n', [Name, Value]),
	print_args(T).
