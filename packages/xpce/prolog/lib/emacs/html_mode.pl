/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
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
