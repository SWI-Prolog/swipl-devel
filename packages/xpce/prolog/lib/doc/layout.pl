/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1999 University of Amsterdam. All rights reserved.
*/

:- module(browser_classes, []).
:- use_module(library(pce)).
:- use_module(doc(util)).
:- use_module(doc(emit)).
:- use_module(doc(vfont)).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Support classes for the html rendering package.  Defines the classes:

	# pbox
	Refinement of class parbox with event-handling, etc.

	# button_box
	hbox for labeling buttons

	# anchor_box
	hbox for labeling anchors

	# bullet_list
	lbox for dealing with bulletted lists (<UL>)

	# enum_list
	lbox for dealing with ordered lists (<OL>)

	# definition_list
	lbox for dealing with definition lists <DL>

	# doc_mode
	Style and font handling
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

		 /*******************************
		 *	       PBOX		*
		 *******************************/

:- pce_begin_class(pbox, parbox, "Refined parbox for viewer").

class_variable(auto_crop, bool, @on).

:- pce_global(@pbox_recogniser,
	      new(click_gesture(left, '', single,
				message(@receiver, clicked, @event)))).

event(PB, Ev:event) :->
	(   send_super(PB, event, Ev)
	;   send(@pbox_recogniser, event, Ev)
	;   send(Ev, is_a, loc_move),
	    send(PB, show_location, Ev)
	).

clicked(PB, Ev:event) :->
	get(PB, locate_event, Ev, Index),
	(   get(PB, box, Index, Box),
	    get(Box, attribute, footnote, Footnote)
	->  send(PB, show_note, Footnote)
	;   find_button(PB, Index, Button),
	    get(Button, message, Message),
	    send(Message, forward, Button)
	;   debugging(parbox),
	    get(PB, box, Index, Box),
	    get(PB, box_area, Index, area(X,Y,W,H)),
	    send(PB, display, box(W, H), point(X, Y))
	).

find_button(PB, Index, Button) :-
	find_button(PB, Index, Index, 100, Button).

find_button(PB, Index0, Index, _, CB) :-
	get(PB, box, Index, CB),
	send(CB, instance_of, button_box),
	get(CB, hypered, open, OB),
	get(PB?content, index, OB, OpenIndex),
	OpenIndex =< Index0, !.
find_button(PB, Index0, Index, MaxDist, CB) :-
	Index1 is Index+1,
	Index0+MaxDist > Index,
	find_button(PB, Index0, Index1, MaxDist, CB).

show_note(_, Note:graphical) :->
	get(Note, size, Size),
	send(Note, lock_object, @on),
	new(W, window('Footnote', size := Size)),
	send(W, kind, popup),
	send(W, display, Note),
	send(W, recogniser,
	     new(C, click_gesture(left, '', single,
				  message(W, free)))),
	send(W, focus, W, C, @nil, @nil),
	get(@event, position, @display, Pos),
	send(W, open, Pos, @on).
	
:- dynamic
	showing/1.		% actually we should use the frame for this

show_location(PB, Ev:event) :->
	get(PB, locate_event, Ev, Index),
	(   find_button(PB, Index, Button),
	    get(Button, balloon, Text),
	    Text \== @nil
	->  true
	;   Text = ''
	),
	(   showing(Text)
	->  true
	;   retractall(showing(_)),
	    assert(showing(Text)),
	    send(PB, report, status, Text)
	).

url(PB, URL:name) :<-
	"Represented URL"::
	get(PB, window, Window),
	catch(get(Window, url, URL), _, fail).

show(PB, Content:prolog, Mode:mode=[doc_mode]) :->
	"Render list of commands in this box"::
%	send(PB, clear),		/* without we can build gradually */
	(   Mode == @default
	->  new(M, doc_mode)
	;   M = Mode
	),
	(   Content = element(_,_,_)
	->  emit([Content], PB, M)
	;   emit(Content, PB, M)
	).

:- pce_end_class.

		 /*******************************
		 *	      BUTTON		*
		 *******************************/

:- pce_begin_class(button_box, hbox,
		   "Open/close buttons").

variable(message,	code*,	 both, "Message executed on click").
variable(balloon,	string*, both, "Message displayed when hooveing").

initialise(B, Message:code*, Balloon:[string]*) :->
	default(Balloon, @nil, Text),
	send_super(B, initialise),
	send(B, message, Message),
	send(B, balloon, Text).

:- pce_end_class.

:- pce_begin_class(anchor_box, hbox,
		   "End-point of a link").

variable(identifier,	name*,	get, "Identifier (label)").

initialise(B, Id:any) :->
	send_super(B, initialise),
	send(B, slot, identifier, Id).

:- pce_end_class.


		 /*******************************
		 *	    BULLET LIST		*
		 *******************************/

:- pce_begin_class(bullet_list, lbox, "TeX bulletlist environment").

class_variable(item_sep, '0..', 0).

new_label(_, Label:graphical) :<-
	new(Label, circle(8)),
	send(Label, fill_pattern, @black_image).

make_item(BL, Item:prolog, Mode:mode, PB:parbox) :<-
	"Create a new item"::
	(   Item == []
	->  Label = @default
	;   get(BL, label_width, LW),
	    new(Label, pbox(LW, right)),
	    get(Mode, clone, Clone),
	    emit(Item, Label, Clone)
	),
	get(Mode, alignment, Align),
	send(BL, append, Label, new(PB, pbox(0, Align))).

:- pce_end_class.


		 /*******************************
		 *	     ENUMERATE		*
		 *******************************/

:- pce_begin_class(enum_list, lbox, "TeX enumerate environment").

variable(index,		int := 0,	both, "Current index").

new_label(EN, Label:graphical) :<-
	get(EN, index, I0),
	I is I0 + 1,
	send(EN, index, I),
	new(Label, text(I, right, normal)).

make_item(BL, Item:prolog, Mode:mode, PB:parbox) :<-
	"Create a new item"::
	(   Item == []
	->  Label = @default
	;   get(BL, label_width, LW),
	    new(Label, pbox(LW, right)),
	    get(Mode, clone, Clone),
	    emit(Item, Label, Clone)
	),
	get(Mode, alignment, Align),
	send(BL, append, Label, new(PB, pbox(0, Align))).

:- pce_end_class.


		 /*******************************
		 *	    DESCRIPTION		*
		 *******************************/

:- pce_begin_class(definition_list, lbox, "HTML <DL> element").

make_item(DL, Item:prolog, Mode:mode, PB:parbox) :<-
	"Create a new item"::
	get(Mode, alignment, Align),
	send(DL, append, @default, new(PB, pbox(0, Align))),
	get(DL, left_margin, LM),
	send(PB, append, hbox(-LM)),
	get(Mode, clone, Clone),
	append(Item, [@br], Label),
	emit([\setfont(weight, bold) | Label], PB, Clone).

:- pce_end_class.


		 /*******************************
		 *	  CLASS DOC-MODE	*
		 *******************************/

:- pce_begin_class(doc_mode, object, "Parameters of rendering engine").

variable(style,		style,	  get,	"Basic text style").
variable(parsep,	hbox,	  get,	"Amount to skip for paragraph").
variable(parindent,	hbox,	  get,	"Indentation for paragraph").
variable(alignment,	name,	  both,	"Default paragraph alignment").
variable(vfont,		vfont,	  get,	"Virtual font").
variable(space,		hbox,	  get,	"Space in current font").
variable(base_url,	name:='', both,	"Reference URL").
variable(ignore_blanks,	[name],	  both,	"How to handle blanks").
variable(link_colour,	colour,	  both, "Colour for hyperlinks").

initialise(M) :->
	send_super(M, initialise),
	send(M, slot, vfont, new(VFont, vfont)),
	send(M, slot, link_colour, colour(dark_green)),
	send(M, slot, style, style(font := VFont?font)),
	send(M, slot, ignore_blanks, @default),
	send(M, set_font, family, helvetica),
	send(M, slot, parsep, hbox(0,8)), 	% vertical skip
	send(M, slot, parindent, hbox(0,0)),	% horizontal skip
	send(M, alignment, justify).

set_font(M, Att:name, Val:any) :->
	"Set a font attribute"::
	get(M, vfont, VFont),
	send(VFont, Att, Val),
	get(VFont, font, Font),
	send(M?style, font, Font),
	get(Font, advance, ' ', SW),
	get(Font, ascent, Ascent),
	get(Font, descent, Descent),
	send(M, slot, space, hbox(SW, Ascent, Descent, @space_rubber)).

colour(M, Colour:colour) :->
	"Set text-foreground colour"::
	send(M?style, colour, Colour).

underline(M, OnOff:bool) :->
	"Set text underline"::
	send(M?style, underline, OnOff).

:- pce_end_class.
