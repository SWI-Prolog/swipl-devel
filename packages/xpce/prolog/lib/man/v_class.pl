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



:- module(man_class,
	[ 
	]).

:- use_module(library(pce)).
:- use_module(util).
:- use_module(v_inherit).
:- require([ forall/2
	   , get_chain/3
	   , send_list/3
	   ]).

:- pce_begin_class(man_class_browser(label), man_frame,
		   "Online manual for a class").

class_variable(value_font,	font,	normal).
class_variable(label_font,	font,	bold).
class_variable(active_font,	font,	italic).

variable(tool_focus,	 class,		get,
	 "Currently displayed class").

		/********************************
		*           CREATION		*
		********************************/

initialise(CB, Manual:man_manual) :->
	"Create from manual"::
	send(CB, send_super, initialise, Manual, 'Class Browser'),
	send(CB, append, new(Dialog, dialog)),
	new(Browser, man_summary_browser(man_summary, size(100, 15))),
	send(new(Pict, dialog), above, Browser),
	send(Browser, right, Dialog),

	fill_dialog(Dialog),
	fill_picture(Pict),

	get(Manual, tool_focus, Focus),
	(   get(@pce, convert, Focus, class, Class)
	->  true
	;   get(@pce, convert, object, class, Class)
	),
	send(CB, tool_focus, Class).


%	dialog(-Dialog)
%
%	Create the dialog window

fill_dialog(D) :-
	get(D, frame, CB),

	send(D, append, label(reporter)),

	send(D, append, new(CI, text_item(class, '',
					  and(message(CB, show_class_name,
						      @arg1),
					      message(@receiver, clear))))),
	send(CI, value_set, ?(@prolog, expand_classname, @arg1)),

	send(D, append, new(FM, menu(filter, toggle))),
	new(FmMsg, message(@manual, user_scope, FM?selection)),
	forall(man_classification(T, L),
	       send(FM, append, menu_item(T, @default, L))),
	send(FM, append,
	     menu_item(all,
		       if(@arg2 == @on,
			  and(message(FM, attribute, saved_selection,
				      FM?selection),
			      message(FM, selection, FM?members),
			      message(FM, modified, @on),
			      FmMsg),
			  and(message(FM, selection,
				      ?(FM, attribute, saved_selection)),
			      message(FM, selected, all, @off),
			      message(FM, modified, @on),
			      FmMsg)))),
	send(FM, message, FmMsg),
	send(FM, columns, 4),
	send(FM, selection, @manual?user_scope),

	send(D, append, new(DM, menu(display, toggle))),
	send(D, append, new(KI, text_item(search, ''))),
	send(D, append, new(SM, menu(field, toggle))),
	send(SM, label, '... In:'),
	send(D, append, graphical(0,0,1,1)), % bit extra spacing
	send(D, append, button(apply,
			       and(message(D, apply),
				   message(CB, apply)))),

	send(KI, advance, none),
	send(CI, advance, none),

	send(DM, layout, horizontal),
	send(DM, columns, 3),
	send_list(DM, append, [ self, sub_class
			      , variable, class_var
			      , send_method, get_method
			      ]),
	send(DM, selection,
	     chain(self, variable, send_method, get_method)),

	send(SM, layout, horizontal),
	send(SM, columns, 2),
	send_list(SM, append, [ name, summary
			      , description /*, diagnostics*/
			      ]),
	send(SM, selection, chain(name, summary)),

	send(D, append, button(help, message(CB, help))),
	send(D, append, button(quit, message(CB, quit))),

	send(D, default_button, apply).


expand_classname(Prefix, Classes) :-
	new(Classes, chain),
	send(@classes, for_all,
	     if(message(@arg1, prefix, Prefix),
		message(Classes, append, @arg1))),
	send(Classes, sort).

fill_picture(P) :-
	send(P, name, window),
	send(P, gap, size(5,3)),
	send(P, hor_stretch, 100),
	send(P, hor_shrink, 100),
	get(P?frame, label_font, Font),
	send(P, append, new(T, label(title, '', Font))),
	send(T, recogniser,
	     click_gesture(left, '', double,
			   message(P?frame, show_initisation_method))),

	send(P, append, new(I, man_inheritance_tree)),
	send(I, name, inheritance).
	

show_initisation_method(CB) :->
	"Show the ->initialise method of the class"::
	get(CB, tool_focus, Class), Class \== @nil,
	get(Class, send_method, initialise, Method),
	send(CB, request_selection, Method, @on).


show_header(CB) :->
	"Show term description in picture window"::
	get(CB, tool_focus, Class),
	get(CB, member, window, P),
	get(P, member, title, Title),
	(   get(Class, man_header, DH), DH \== @nil
	->  send(Title, selection, DH)
	;   send(Title, selection, '??')
	).


show_inheritance(CB) :->
	"Show inheritance path in picture"::
	get(CB, member, window, P),
	get(P, member, inheritance, Tree),
	get(CB, tool_focus, Class),
	send(Tree, show, Class),
	send(P, layout),
	send(P, compute),
	get(P, bounding_box, area(_X, Y, _W, H)),
	get(P, gap, size(_GW, GH)),
	get(P, pen, Pen),
	IH is Y+H+GH+2*Pen,
	(   get(P?tile, ideal_height, OIH),
	    OIH < IH
	->  send(P?tile, ideal_height, IH),
	    send(CB, resize)
	;   send(P?tile, ideal_height, IH)
	).


scope(CB, Scope:chain) :<-
	"Inheritance scope"::
	get(CB, member, window, P),
	get(P, member, inheritance, Fig),
	get(Fig, scope, Scope).


		/********************************
		*         COMMUNICATION		*
		********************************/

request_tool_focus(CB, Obj:object*, _Force:[bool]) :->
	"Do not broadcast a request"::
	send(CB, tool_focus, Obj).


tool_focus(CB, Obj:object*) :->
	"Switch to specified focus"::
	(   get(@pce, convert, Obj, class, Class)
	->  true
	;   get(Obj, context, Class),
	    send(Class, instance_of, class)
	->  true
	;   fail
	),
	send(CB, slot, tool_focus, Class),
	get(Class, name, ClassName),
	atom_concat('Class ', ClassName, Label),
	send(CB, label, Label, ClassName),
	send(CB, show_header),
	send(CB, show_inheritance),
	send(CB, apply).


selected(CB, Obj:object*) :->
	"Set the selection"::
	send(CB?man_summary_browser_member, selected, Obj).


release_selection(CB) :->
	"Stop having the selection"::
	send(CB?man_summary_browser_member, release_selection).


		/********************************
		*            BROWSER		*
		********************************/

apply(CB) :->
	"Fill the browser window"::
	get(CB, dialog_member, Dialog),
	get(CB, tool_focus, Class),
	get(CB, man_summary_browser_member, Browser),
	get_chain(Dialog?display_member, selection, Types),
	get_chain(Dialog?field_member, selection, Fields),
	get(Dialog?search_member, selection, Keyword),
	get(CB, scope, Scope),

	send(Browser, clear),
	send(CB, report, progress, 'Searching ...'),
	apropos_class(Class, Scope, Types, Fields, Keyword, Matches),
	send(CB, report, done),
	send(Browser, members, Matches),
	send(CB, keyboard_focus, Browser),
	send(Dialog?apply_member, active, @off).
	

activate_apply(CB) :->
	"Activate the apply button"::
	get(CB, member, dialog, Dialog),
	get(Dialog, member, apply, Button),
	send(Button, active, @on).


show_class_name(CB, Name:name) :->
	"Switch to named class"::
	(   get(@classes, member, Name, Class)
	->  send(CB, request_tool_focus, Class)
	;   send(CB, report, progress, 'Trying to autoload ...'),
	    send(@pce, exception, undefined_class, Name),
	    get(@classes, member, Name, Class),
	    send(CB, report, done)
	->  send(CB, request_tool_focus, Class)
	;   send(CB, report, warning, 'No such class: %s', Name),
	    fail
	).


:- pce_end_class.

