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
:- use_module(library(pce)).
:- use_module(library(lists)).
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
	   open(true),
	   write_options([ quoted(true),
			   portray(true),
			   numbervars(true)
			 ])
	 ]).

tv(Term, Attributes) :-
	attribute(Attributes, view(V)),
	if(Attributes, clear(true), send(V, clear)),
	if(Attributes, open(true), send(V, open)),
	if(Attributes, comment(Comment), send(V, label, Comment)),
	if(Attributes, source_object(Frag), send(V, source_object, Frag)),
	get(V, text_buffer, TB),
	pce_open(TB, write, Fd),
	print_term(Term, [output(Fd)|Attributes]),
	close(Fd),
	send(V, caret, 0),
	send(V, editable, @off),
	if(Attributes, write_options(WrtOpts), send(V, show_options, WrtOpts)).


attribute(Attributes, A) :-
	memberchk(A, Attributes).

if(Attributes, Cond, Goal) :-
	memberchk(Cond, Attributes), !,
	Goal.
if(_, _, _).

:- pce_global(@view_term, new(term_viewer)).

:- pce_begin_class(term_viewer, frame,
		   "Pretty-print a Prolog term").

initialise(TV) :->
	send_super(TV, initialise),
	send(TV, append, new(TD, dialog)),
	send(new(view), below, TD),
	send(TD, border, size(0,2)),
	send(TD, append, new(M, menu(options, toggle,
				     message(TV, update)))),
	send(M, layout, horizontal),
	send_list(M, append,
		  [ portray,
		    quoted,
		    max_depth
		  ]),
	send(TD, append, int_item(max_depth, 10,
				  message(TV, update), 1),
	     right).

clear(TV) :->
	get(TV, member, view, View),
	send(View, clear).

text_buffer(TV, TB:text_buffer) :<-
	get(TV, member, view, View),
	get(View, text_buffer, TB).

caret(TV, Caret:int) :->
	get(TV, member, view, View),
	send(View, caret, Caret).

editable(TV, E:bool) :->
	get(TV, member, view, View),
	send(View, editable, E).

source_object(TV, Obj:object) :->
	send(TV, delete_hypers, source),
	new(_, hyper(TV, Obj, source, view)).

update(TV) :->
	get(TV, member, dialog, D),
	get(D, member, options, Menu),
	get(Menu, selection, Options),
	make_options([ portray,
		       quoted
		     ], Options, OptionList0),
	get(D, member, max_depth, DepthItem),
	(   send(Options, member, max_depth),
	    send(DepthItem, active, @on),
	    get(DepthItem, selection, MaxDepth)
	->  OptionList = [max_depth(MaxDepth)|OptionList0]
	;   send(DepthItem, active, @off),
	    OptionList = OptionList0
	),
	get(TV, hypered, source, Source),
	get(Source, value, Term),
	tv(Term,
	   [ view(TV),
	     clear(true),
	     write_options(OptionList)
	   ]).
	
make_options([], _, [ numbervars(true), attributes(portray) ]).
make_options([H0|T0], Selection, [H|T]) :-
	(   send(Selection, member, H0)
	->  V = true
	;   V = false
	),
	H =.. [H0,V],
	make_options(T0, Selection, T).

show_options(V, Options:prolog) :->
	"Show current option values"::
	get(V, member, dialog, D),
	get(D, member, options, Menu),
	send(Menu, selected, max_depth, @off),
	get(D, member, max_depth, DepthItem),
	send(DepthItem, active, @off),
	(   member(Option, Options),
	    functor(Option, Name, 1),
	    get(Menu, member, Name, Item),
	    arg(1, Option, Value),
	    (	Name == max_depth
	    ->	send(Item, selected, @on),
		send(DepthItem, active, @on),
		send(DepthItem, selection, Value)
	    ;	send(Item, selected, Value)
	    ),
	    fail
	;   true
	).

:- pce_end_class.
