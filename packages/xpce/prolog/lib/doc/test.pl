/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
