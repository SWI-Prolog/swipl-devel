/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(emacs_html_mode, []).
:- use_module(library(pce)).
:- require([ send_list/3
	   ]).

:- op(100, fx, #).			% `DTD' attributes
:- op(100, fy, *).
:- op(100, fy, ?).
:- op(100, fy, +).

:- pce_global(@html_paragraph_end_regex,
	      make_parent_regex).

make_parent_regex(R) :-
	findall(P, par(P), Ps),
	concat_atom(Ps, '\\|', P0),
	concat_atom(['\\s *\\($\\|<\\(', P0, '\\)\\)'], P1),
	new(R, regex(P1)),
	send(R, ignore_case, @on).

par('h[1-4]').
par('dl').
par('ul').
par('ol').
par('li').
par('dt').
par('tr').
par('p').
par('hr').
par('table').
par('blockquote').


:- emacs_begin_mode(html, language,
		    "Mode for editing HTML documents",
		    [ open_document     = key('\\C-c\\C-o') + button('HTML')
		    ],
		    [ '<'  = open_bracket('>'),
		      paragraph_end(@html_paragraph_end_regex)
		    ]).

:- pce_global(@html_end_element_regex, new(regex('</\\(\\w+\\)[^>]*>'))).

setup_mode(E) :->
	"Switch editor into fill-mode"::
	send(E, fill_mode, @on).

		 /*******************************
		 *	      TYPING		*
		 *******************************/

insert_self(M, Times:[int], Id:[char]) :->
	"Insert, but donot warn on mismatched bracket"::
	pce_catch_error(chain(no_matching_bracket, mismatched_bracket),
			send(M?editor, insert_self, Times, Id)).


		 /*******************************
		 *	      DOCUMENT		*
		 *******************************/

open_document(M) :->
	"Insert document header"::
	send(M, format,
	     '<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 3.2//EN">\n\n'),
	send(M, make_element, html),
	send(M, make_element, head),
	send(M, make_element, title),
	get(M, caret, Caret),
	send(M, out),			% out of the title
	send(M, out),			% out of the head
	send(M, make_element, body),
	send(M, out),			% hack
	send(M, format, '\n'),
	send(M, caret, Caret).


		 /*******************************
		 *	      ELEMENTS		*
		 *******************************/

make_element(M, E:name) :->
	"Insert element at location"::
	(   cdata_element(E)
	->  send(M, format, '<%s>', E),
	    get(M, caret, Caret),
	    send(M, format, '</%s>', E)
	;   send(M, format, '<%s>\n', E),
	    get(M, caret, Caret),
	    send(M, format, '\n</%s>', E)
	),
	send(M, caret, Caret).

out(M) :->
	"Get to the end of this element"::
	get(M, text_buffer, TB),
	get(M, caret, Caret),
	get(@html_end_element_regex, search, TB, Caret, _Start),
	get(@html_end_element_regex, register_end, 0, End),
	send(M, caret, End),
	(   send(M, looking_at, '\\s *$')
	->  send(M, forward_char)
	;   true
	).

		 /*******************************
		 *	    ELEMENT DEFS		*
		 *******************************/

cdata_element(E) :-
	element(E, Content, _Attributes),
	memberchk(#cdata, Content), !.

element(html,  [head, body],   []).
element(head,  [title, meta*], []).
element(body,  [],	       []).
element(title, [#cdata],       []).

:- emacs_end_mode. 
