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

:- module(prolog_term_view,
	  [ view_term/1,		% +Term
	    view_term/2			% +Term, +Attributes
	  ]).
:- use_module(pprint).

view_term(Term) :-
	view_term(Term, []).

view_term(Term, _) :-			% TBD: Turn into user hook!
	object(Term), !,
	manpce,
	send(@manual, inspect, Term).
view_term(Term, Attributes0) :-
	defaults(Defs),
	append(Attributes0, Defs, Attributes),
	tv(Term, Attributes).

defaults([ view(@view_term),
	   clear(true),
	   open(true)
	 ]).

tv(Term, Attributes) :-
	attribute(Attributes, view(V)),
	if(Attributes, clear(true), send(V, clear)),
	if(Attributes, open(true), send(V, open)),
	if(Attributes, comment(Comment), send(V?frame, label, Comment)),
	get(V, text_buffer, TB),
	pce_open(TB, write, Fd),
	print_term(Term, [output(Fd)|Attributes]),
	close(Fd),
	send(V, caret, 0),
	send(V, editable, @off),
	send(V?text_cursor, displayed, @off).

attribute(Attributes, A) :-
	memberchk(A, Attributes).

if(Attributes, Cond, Goal) :-
	memberchk(Cond, Attributes), !,
	Goal.
if(_, _, _).

:- pce_global(@view_term, new(term_viewer)).

:- pce_begin_class(term_viewer, view,
		   "Pretty-print a Prolog term").


:- pce_end_class.
