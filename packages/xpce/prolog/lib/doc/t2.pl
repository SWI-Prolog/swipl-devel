/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam. All rights reserved.
*/

:- use_module(xml_browse).

show :-
	show('t.html').

show(File) :-
	load_html_file(File, Tokens),
	new(W, xml_browser(Tokens)),
	send(W, open).
