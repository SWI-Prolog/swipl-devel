/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- module(doc_test,
	  [ test/0,
	    test/1			% +URL
	  ]).
:- use_module(library('doc/load')).
:- use_module(doc(url_fetch)).		% avoid autoload for debugging

test :-
	test('http://localhost/').

test(URL) :-
	send(new(B, doc_browser), open),
	send(B, url, URL).
