/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1992 University of Amsterdam. All rights reserved.
*/



:- module(man_class,
	[ 
	]).

:- use_module(library(pce)).
:- use_module(util).
:- require([ concat/3
	   , get_chain/3
	   , send_list/3
	   ]).

:- pce_begin_class(man_class_browser(label), man_frame,
		   "Online manual for a class").

variable(tool_focus,	 class,		get,
	 "Currently displayed class").

		/********************************
		*           CREATION		*
		********************************/

initialise(CB, Manual:man_manual) :->
	"Create from manual"::
	send(CB, send_super, initialise, Manual, 'Class Browser'),

	dialog(Dialog),
	picture(Pict),
	new(Browser, man_summary_browser(man_summary, size(90, 15))),

	send(CB, append, Dialog),
	send(Pict, above, Browser),
	send(Browser, right, Dialog),

	get(Manual, tool_focus, Focus),
	(   get(@pce, convert, Focus, class, Class)
	->  true
	;   get(@pce, convert, object, class, Class)
	),
	send(CB, tool_focus, Class).


%	dialog(-Dialog)
%
%	Create the dialog window

dialog(D) :-
	new(D, dialog),

	new(CB, D?frame),		       % the class browser

	send(D, append, new(label)),	       % reporter
	send(D, append, new(DM, menu(display, toggle))),
	send(D, append, new(CI, text_item(class, '',
					  and(message(CB, show_class_name,
						      @arg1),
					      message(@receiver, clear))))),
	send(CI, value_set, ?(@prolog, expand_classname, @arg1)),
	send(D, append, new(SM, menu(field, toggle))),
	send(D, append, new(KI, text_item(search, ''))),
	send(D, append, new(graphical(height := 15))), % just add some space
	send(D, append, new(IM, menu(scope, marked))),
	send(D, append, new(graphical(height := 15))), % and once more
	send(D, append, button(apply,
			       and(message(D, apply),
				   message(CB, fill_browser)))),

	send(KI, advance, none),
	send(CI, advance, none),

	send(IM, layout, horizontal),
	send_list(IM, append, [super, own, sub]),
	send(IM, auto_value_align, @off),
	send(IM, selection, own),

	send(DM, layout, horizontal),
	send(DM, columns, 3),
	send_list(DM, append, [ self, sub_class
			      , variable, resource
			      , send_method, get_method
			      ]),
	send(DM, selection, chain(self, variable)),

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
	     if(message(@arg2?name, prefix, Prefix),
		message(Classes, append, @arg2))).

picture(P) :-
	new(P, dialog),
	send(P, name, window),
	send(P, gap, size(15, 3)),
	send(P, hor_stretch, 100),
	send(P, hor_shrink, 100),
	send(P, append, new(T, label(title, '', font(helvetica, bold, 14)))),
	send(T, recogniser,
	     click_gesture(left, '', double,
			   message(P?frame, show_initisation_method))),
	send(P, append, new(F, figure)),
	send(F, name, inheritance),
	send(F, display, text(inheritance)), % for layout
	send(F, format, format(vertical, 1, @on)),
	send(F, format, adjustment, vector(center)),
	send(F, format, row_sep, 0),
	send(P, append, label(delegation, '', font(helvetica, roman, 14))).
	

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
	get(P, member, inheritance, Fig),
	send(Fig, clear),
	get(CB, tool_focus, Class),
	super_chain(Class, Fig).


show_delegation(CB) :->
	"Show delegation slots in picture"::
	get(CB, member, window, Win),
	get(Win, member, delegation, Label),
	get(CB, tool_focus, Class),
	get(Class, man_delegate_header, Str),
	(   get(Str, size, 0)
	->  send(Label, selection, '(No delegation)')
	;   send(Label, selection, string('Delegation: %s', Str))
	).


:- pce_global(@man_class_text_handler, make_class_text_handler).

make_class_text_handler(H) :-
	new(H, click_gesture(left, '', double,
			     and(message(@receiver, inverted, @on),
				 message(@receiver, flush),
				 message(@receiver?frame,
					 request_tool_focus,
					 ?(@pce, convert,
					   @receiver?string, class))))).

%	super_chain(+Class, +Fig)

super_chain(@nil, _) :- !.
super_chain(Class, Fig) :-
	get(Class, super_class, Super),
	super_chain(Super, Fig),
	send(Fig, display, line(0, 5, 20, 5, second)),
	send(Fig, display, new(T, text(Class?name, left,
				       font(helvetica, roman, 14)))),
	send(T, recogniser, @man_class_text_handler).

	
		/********************************
		*         COMMUNICATION		*
		********************************/

request_tool_focus(CB, Obj:object*) :->
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
	concat('Class ', ClassName, Label),
	send(CB, label, Label, ClassName),
	send(CB, show_header),
	send(CB, show_inheritance),
	send(CB, show_delegation),
	send(CB, fill_browser).


selected(CB, Obj:object*) :->
	"Set the selection"::
	send(CB?man_summary_browser_member, selected, Obj).


release_selection(CB) :->
	"Stop having the selection"::
	send(CB?man_summary_browser_member, release_selection).


		/********************************
		*            BROWSER		*
		********************************/

fill_browser(CB) :->
	"Fill the browser window"::
	get(CB, dialog_member, Dialog),
	get(CB, tool_focus, Class),
	get(CB, man_summary_browser_member, Browser),
	get_chain(Dialog?display_member, selection, Types),
	get_chain(Dialog?field_member, selection, Fields),
	get(Dialog?search_member, selection, Keyword),
	get(Dialog?scope_member, selection, Inherit),

	send(Browser, clear),
	send(CB, report, progress, 'Searching ...'),
	apropos_class(Class, Inherit, Types, Fields, Keyword, Matches),
	send(CB, report, done),
	send(Browser, members, Matches),
	send(Dialog?apply_member, active, @off).
	

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

