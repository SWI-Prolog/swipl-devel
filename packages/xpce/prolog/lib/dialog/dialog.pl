/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Toplevel file of the  XPCE/Prolog  dialog   window  editor.   This  file
defines the XPCE class dia_editor (subclass  of class frame), from which
you may create an instances and open them.  

The dialog editor is normally started through  the file dialog.pl in the
standard XPCE library directory.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- module(dia_dialog, []).
:- use_module(library(pce)).
:- use_module(library(pce_prompter)).
:- [ proto,
     layout,
     generate,
     pretty_print,
     behaviour,
     load,
     util
   ].
:- use_module(library(pce_template)).
:- require([ between/3
	   , default/3
	   , forall/2
	   , ignore/1
	   , maplist/3
	   , member/2
	   , pce_help_file/2
	   , pce_image_directory/1
	   , postscript/2
	   , send_list/3
	   , tmp_file/2
	   ]).


:- pce_autoload(drag_and_drop_gesture, library(dragdrop)).
:- pce_autoload(drag_and_drop_dict_item_gesture, library(dragdict)).
:- pce_autoload(dia_attribute_editor, library('dialog/attribute')).
:- pce_autoload(finder, library(find_file)).
:- pce_global(@finder, new(finder)).

dia_version('0.7').

:- initialization pce_help_file(dialog, pce_help('dialog.hlp')).
:- initialization pce_image_directory(library('dialog/bitmaps')).


		 /*******************************
		 *	  PROTO TEMPLATE	*
		 *******************************/

:- pce_begin_class(dia_proto, template).

variable(can_resize,	  bool := @off,	both,	"Object can be resized").
variable(proto,		  name,		both,	"Name of proto description").
variable(ref_indicator,	  graphical*,	get,	"Indicate reference-point").
variable(fixed_alignment, bool := @off,	both,	"<-alignment is specified").

:- free(@dia_proto_recogniser).
:- pce_global(@dia_proto_recogniser, make_dia_proto_recogniser).

make_dia_proto_recogniser(R) :-
	new(DI, @event?receiver),
	new(Dev, DI?device),
	new(IsTarget, message(Dev, instance_of, dia_target_dialog)),
	new(CanResize, @event?receiver?can_resize == @on),

	new(R, handler_group(
		new(RG, resize_gesture(left)),
		new(_DG, drag_and_drop_gesture(left, '', @off)),
		new(_,  click_gesture(left, '', single,
				      message(DI, help_ui))),
		new(EG, click_gesture(left, '', double,
				      message(DI, edit_attributes))))),
	send(RG, condition, and(CanResize, IsTarget)),
	send(EG, condition, IsTarget).


event(DI, Ev:event) :->
	"Process event"::
	(   get(DI?device, mode, run)
	->  send(DI, send_super, event, Ev)
	;   send(@dia_proto_recogniser, event, Ev)
	).


help_ui(DI) :->
	"Provide simple help"::
	send(DI, report, status,
	     'Use double-left-click to edit; left-drag to move or copy').


behaviour_model(DI, Model:object) :<-
	"Related behaviour model"::
	get(DI, hypered, behaviour_model, Model).


edit_attributes(DI) :->
	"Start attribute editor"::
	get(DI?window, attribute_editor, Ed),
	get(DI?window, mode, Mode),
	get(DI, proto, Proto),
 	findall(Att, attribute(Mode, Proto, Att), Atts),
	(   Atts == []
	->  send(DI, report, warning, 'No attributes in "%s" mode', Mode)
	;   maplist(att_to_pce, Atts, PceAtts),
	    VectorTerm =.. [vector|PceAtts],
	    send(Ed, send_vector, client, DI, VectorTerm),
	    send(Ed, open),
	    send(Ed, expose)
	).
	
att_to_pce(Atom, Atom) :-
	atom(Atom), !.
att_to_pce(Term, tuple(Att, Cond)) :-
	Term =.. [Att, Cond].


reference_x(DI, X:'0..100') :->
	"X-coordinate of <->reference"::
	get(DI?area, width, W),
	RX is (W*X)//100,
	(   get(DI, reference, Ref)
	->  send(Ref, x, RX),
	    send(DI, send_class, reference, Ref) % avoid addressing attribute
	;   send(DI, reference, point(RX, 0))
	).
reference_x(DI, X:'0..100') :<-
	"X-coordinate of <->reference"::
	(   get(DI?reference, x, RX)
	->  get(DI?area, width, W),
	    X is (100*RX) // W
	;   X = 0
	).
reference_y(DI, Y:'0..100') :->
	"Y-coordinate of <->reference"::
	get(DI?area, height, H),
	RY is (H*Y)//100,
	(   get(DI, reference, Ref)
	->  send(Ref, y, RY),
	    send(DI, send_class, reference, Ref) % avoid addressing attribute
	;   send(DI, reference, point(0, RY))
	).
reference_y(DI, Y:'0..100') :<-
	"Y-coordinate of <->reference"::
	(   get(DI?reference, y, RY)
	->  get(DI?area, height, H),
	    Y is (100*RY) // H
	;   Y = 0
	).


fixed_reference(DI, Val:bool) :->
	"Floating or fixed reference position"::
	(   Val == @on
	->  (	get(DI, reference, Ref)
	    ->	send(DI, reference, Ref)
	    ;	send(DI, reference, point(0,0))
	    )
	;   send(DI, reference, @default)
	).
fixed_reference(DI, Val:bool) :<-
	"Is reference fixed"::
	get(DI, slot, reference, Ref),
	(   Ref == @default
	->  Val = @off
	;   Val = @on
	).


geometry(DI, X:[int], Y:[int], W:[int], H:[int]) :->
	"Handle ref-indicator"::
	send(DI, send_super, geometry, X, Y, W, H),
	update_indicator_position(DI).


update_indicator_position(DI, Ind) :-
	get(DI, displayed, @on),
	get(DI, reference, point(RX, RY)), !,
	send(Ind, displayed, @on),
	send(Ind, center, point(DI?x + RX, DI?y + RY)).
update_indicator_position(_DI, Ind) :-
	send(Ind, displayed, @off).

update_indicator_position(DI) :-
	get(DI, ref_indicator, Ind),
	(   Ind \== @nil
	->  update_indicator_position(DI, Ind)
	;   true
	).

displayed(DI, Val:bool) :->
	"Handle ref-indicator"::
	send(DI, send_super, displayed, Val),
	update_indicator_position(DI).

	
device(DI, Dev:device*) :->
	"Handle ref-indicator"::
	send(DI, send_super, device, Dev),
	(   get(DI, ref_indicator, Ind), Ind \== @nil
	->  (	Dev == @nil
	    ->	send(Ind, device, @nil)
	    ;	send(Ind, device, Dev?overlay),
		update_indicator_position(DI)
	    )
	;   true
	).


reference(DI, Ref:[point]) :->
	"Handle ref-indicator"::
	send(DI, send_super, reference, Ref),
	update_indicator_position(DI).


ref_indicator(DI, Ind:graphical*) :->
	"Change reference indication"::
	(   get(DI, ref_indicator, Old),
	    Old \== @nil
	->  send(Old, device, @nil)
	;   true
	),
	send(DI, slot, ref_indicator, Ind),
	(   Ind \== @nil
	->  send(Ind, device, DI?device?overlay),
	    update_indicator_position(DI, Ind)
	;   true
	).
		 
		 /*******************************
		 *    ASSOCIATED POPUP MENU	*
		 *******************************/

popup_items(B, Items:chain) :->
	(   send(Items, empty)
	->  send(B, popup, @nil)
	;   (   get(B, popup, P), P \== @nil
	    ->  true
	    ;   send(B, popup, new(P, popup))
	    ),
	    send(P, clear),
	    send(Items, for_all, message(P, append, @arg1))
	).
popup_items(B, Items:chain) :<-
	(   get(B, popup, P), P \== @nil
	->  get(P, members, Items)
	;   new(Items, chain)
	).
	    
clear_popup(B) :->
	(   get(B, popup, P), P \== @nil
	->  send(P, clear)
	;   true
	).

has_popup(B, V:bool) :->
	(   V == @off
	->  send(B, popup, @nil)
	;   (	get(B, popup, P), P \== @nil
	    ->	true
	    ;	send(B, popup, new(popup))
	    )
	).
has_popup(B, V:bool) :<-
	(   get(B, popup, P), P \== @nil
	->  V = @on
	;   V = @off
	).

:- pce_end_class.


:- pce_begin_class(dia_proto_menu, menu).

members(M, Items:chain) :->
	send(M, clear),
	send(Items, for_all, message(M, append, @arg1)).

feedback(M, Feedback:{box,invert,image}) :->
	"Limit available feedbacks"::
	(   Feedback == image
	->  send(M, on_image,  ?(M, resource_value, on_image)),
	    send(M, off_image, ?(M, resource_value, off_image))
	;   send(M, on_image,  @nil),
	    send(M, off_image, @nil)
	),
	(   Feedback == box
	->  send(M, pen, 1)
	;   send(M, pen, 0)
	),
	send(M, send_super, feedback, Feedback).

:- pce_end_class.

:- pce_begin_class(dia_proto_menu_item, menu_item,
		   "Editable menu-item").

variable(proto,		name := menu_item,	get, "Prototype").

name(MI, Name:name) :<-
	get(MI, print_name, Str),
	get(Str, value, Name).

behaviour_model(DI, Model:object) :<-
	"Related behaviour model"::
	get(DI, hypered, behaviour_model, Model).

:- pce_end_class.

:- pce_begin_class(dia_proto_text_item, text_item).

dia_argument_type(TI, Selector:name, Type:type) :<-
	"The type of the <-selection is <-type"::
	(   Selector == selection
	->  get(TI, type, Type)
	;   get(TI, get_super, dia_argument_type, Type)
	).

:- pce_end_class.

:- pce_begin_class(dia_proto_list_browser, list_browser,
		   "Editable list_browser").

name(LB, Name:name) :->
	"Update ->label too"::
	send(LB, send_super, name, Name),
	(   get(LB, show_label, @on)
	->  get(Name, label_name, LabelName),
	    send(LB, label, LabelName)
	;   true
	).


dia_argument_type(LB, Selector:name, Type:type) :<-
	"The type of the <-selection"::
	(   Selector == selection
	->  (	get(LB, multiple_selection, @on)
	    ->	get(@pce, convert, chain, type, Type)
	    ;	get(@pce, convert, 'dict_item*', type, Type)
	    )
	;   get(LB, get_super, dia_argument_type, Type)
	).

:- pce_end_class.


make_proto_class(ClassSpec, ProtoClassName) :-
	get(@pce, convert, ClassSpec, class, Class),
	get(Class, name, ClassName),
	get(dia_proto_, append, ClassName, ProtoClassName),
	new(ProtoClass, class(ProtoClassName, Class)),
	use_class_template(ProtoClass, dia_proto).


		 /*******************************
		 *	      TARGET		*
		 *******************************/

:- pce_begin_class(dia_dialog, dialog, "Editable dialog").

variable(mode, {create,layout,action,run} := create, get, "Current mode").
variable(overlay, device, get, "Overlay for feedback stuff").

initialise(D, Label:[name], Size:[size]) :->
	send(D, send_super, initialise, Label, Size),
	send(D, display, new(Overlay, device)),
	send(D, slot, overlay, Overlay),
	send(Overlay, auto_align, @off).

:- pce_end_class.


:- pce_begin_class(dia_target_dialog, dia_dialog, "Target dialog").

resource(size,	size, size(400, 200)).

initialise(D, Label:[name], Size:[size]) :->
	default(Size, resource(D, size), Sz),
	send(D, send_super, initialise, Label),
	send(D, size, Sz).		% force explicit size


unlink(D) :->
	"Remove from editor"::
	(   get(D, editor, Editor)
	->  send(Editor, deleted_target, D)
	;   true
	),
	send(D, send_super, unlink).


report_to(D, Obj:visual) :<-
	"Delegate errors to the editor"::
	(   get(D, mode, run)
	->  get(D, get_super, report_to, Obj)
	;   get(D, editor, Obj)
	).


editor(D, Editor:frame) :<-
	"The dialog editor"::
	get(D, hypered, editor, Editor).


drop(D, DI:graphical, Pos:point) :->
	(   get(DI, device, D)		% move
	->  send(DI, position, Pos)
	;   send(DI, instance_of, msg_object)
	->  get(DI, ui_object, UI),
	    send(D, drop, UI, Pos)
	;   get(DI, clone, Clone),	% import
	    send(Clone, lock_object, @on),
	    (	get(DI, device, @nil)	% source has no device
	    ->  get(Clone, proto, Proto),
		proto(Proto, _, _, Actions),
		forall(member(Action, Actions),
		       (   Action = (Selector := Arg)
		       ->  send(Clone, Selector, Arg)
		       ;   send(Clone, Action)
		       )),
		get(Clone?area, width, W0),
		(   send(Clone, has_send_method, label_width)
		->  ignore(send(Clone, label_width, @default))
		;   true
		),
		get(Clone?area, width, W1),
		send(Pos, x, Pos?x + W0-W1)
	    ;	true			% copy from other dialog
	    ),
	    send(D, display, Clone, Pos),
	    send(Clone, lock_object, @off),
	    send(D?overlay, expose),
	    (   get(D, mode, layout)
	    ->  send(Clone, ref_indicator, bitmap(@mark_handle_image))
	    ;   true
	    )
	).
	    

preview_drop(D, DI:graphical*, Pos:[point]) :->
	(   DI == @nil			% stop the preview
	->  (   get(D, attribute, preview_outline, OL)
	    ->	send(OL, device, @nil),
		send(D, delete_attribute, preview_outline)
	    ;	true
	    )
	;   get(D, attribute, preview_outline, OL) % moving outline
	->  send(OL, position, Pos)
	;   (   send(DI, instance_of, msg_object)
	    ->	get(DI, ui_object, UI)
	    ;	UI = DI
	    ),
	    get(UI?area, size, size(W, H)), % move or import
	    send(D, attribute, preview_outline, new(OL, box(W, H))),
	    send(OL, texture, dotted),
	    send(D, display, OL, Pos)
	).



:- pce_global(@dia_dialog_recogniser, make_dia_dialog_recogniser).
:- free(@dia_dialog_recogniser).

make_dia_dialog_recogniser(G) :-
	D = @receiver, 
	new(NoRunMode, @arg1?receiver?(mode) \== run), % @arg1 is the event
	new(G, handler_group(new(C, click_gesture(left, '', single,
						  message(D, make_current))),
			     new(A, click_gesture(left, '', double,
						  message(D,
							  edit_attributes))))),
	send(C, condition, NoRunMode),
	send(A, condition, NoRunMode).


event(D, Ev:event) :->
	(   send(D, send_super, event, Ev)
	->  true
	;   send(@dia_dialog_recogniser, event, Ev)
	).


make_current(D) :->
	"Make this dialog editor the current one"::
	get(D, editor, Editor),
	send(Editor, target, D),
	send(Editor, mode, D?(mode)).


edit_attributes(D) :->
	"Edit window attributes"::
	get(D, proto, Proto),
	get(D, mode, Mode),
	get(D, attribute_editor, Ed),
	findall(Att, attribute(Mode, Proto, Att), Atts),
	(   Atts == []
	->  send(D, report, warning,
		 'No attributes in "%s" mode', Mode)
	;   maplist(att_to_pce, Atts, PceAtts),
	    VectorTerm =.. [vector|PceAtts],
	    send(Ed, send_vector, client, D, VectorTerm),
	    send(Ed, open)
	).


proto(_D, Proto:name) :<-
	"Prototype-name (for attribute editor)"::
	Proto = dialog.


gap_x(D, G:'0..') :->
	send(D?gap, width, G).
gap_y(D, G:'0..') :->
	send(D?gap, height, G).
gap_x(D, G:'0..') :<-
	get(D?gap, width, G).
gap_y(D, G:'0..') :<-
	get(D?gap, height, G).


name(D, Name:name) :->
	get(D, name, OldName),
	send(D, send_super, name, Name),
	(   get(D, hypered, editor, Editor)
	->  get(Editor, member, browser, Browser),
	    get(Browser, member, OldName, DI),
	    send(DI, key, Name)
	;   true
	),
	get(Name, label_name, Label),
	send(D, frame_label, Label).

frame_label(D, Label:name) :->
	send(D?frame, label, Label).
frame_label(D, Label:name) :<-
	get(D?frame, label, Label).


mode(D, Mode:{create,layout,action,run}) :->
	"Switch current mode"::
	(   get(D, mode, Mode)
	->  true
	;   send(D, slot, mode, Mode),
	    (   Mode == run
	    ->  send(D, keyboard_focus, @nil),
		send(D, advance)
	    ;   true
	    ),
	    fix_layout_mode(D, Mode)
	).

fix_layout_mode(D, Mode) :-
	(   Mode == layout
	->  Mark = create(bitmap, @mark_handle_image)
	;   Mark = @nil
	),
	get(D, overlay, Ovl),
	send(D?graphicals, for_all,
	     if(@arg1 \== Ovl, message(@arg1, ref_indicator, Mark))).


attribute_editor(D, Ed:dia_attribute_editor) :->
	new(_, dia_transient_hyper(D, Ed, attribute_editor, target_dialog)).

attribute_editor(D, Ed:dia_attribute_editor) :<-
	"Lookup existing or create editor"::
	(   get(D, hypered, attribute_editor, Ed)
	->  true
	;   new(Ed, dia_attribute_editor('Attribute Editor')),
	    send(Ed, confirm_done, @off),
	    send(D, attribute_editor, Ed)
	).

		 /*******************************
		 *	   LAYOUT STUFF		*
		 *******************************/

fix_layout(D) :->
	"Fix the layout"::
	send(D, save_dialog_layout),
	layout_dialog(D).


save_dialog_layout(D) :->
	"Save layout for later ->undo"::
	get(D, overlay, Overlay),
	send(D?graphicals, for_all,
	     if(@arg1 \== Overlay,
		message(@arg1, attribute,
			undo_position, @arg1?area?position))).

restore_dialog_layout(D) :->
	"Restore saved dialog layout"::
	new(UndoPos, ?(@arg1, attribute, undo_position)),
	(   get(D?graphicals, find, UndoPos, _)
	->  get(D, overlay, Overlay),
	    send(D?graphicals, for_all,
		 if(@arg1 \== Overlay,
		    and(message(@arg1, above, @nil),
			message(@arg1, below, @nil),
			message(@arg1, left,  @nil),
			message(@arg1, right, @nil),
			if(message(@arg1, has_send_method, label_width),
			   if(message(@arg1, label_width, @default))),
			if(message(@arg1, position,
				   ?(@arg1, attribute, undo_position))))))
	;   send(D, report, warning, 'No undo information')
	).


fit_size(D) :->
	"Recompute layout and update size"::
	get(D, bounding_box, area(_, _, W, H)),
	get(D, gap, size(GW, GH)),
	NW is W+2*GW,
	NH is H+2*GH,
	send(D, size, size(NW, NH)).


		 /*******************************
		 *     SOURCE CODE GENERATION	*
		 *******************************/

pretty_print(Term, String) :-
	tmp_file(xpcepp, TmpNam),
%	get(string('/tmp/xpce-pp-%d', @pce?pid), value, TmpNam),
	telling(Old), tell(TmpNam),
	pretty_print(Term),
	told, tell(Old),
	new(F, file(TmpNam)),
	send(F, open, read),
	get(F, read, String),
	send(F, remove),
	send(F, free).


prolog_source(D, String:string) :<-
	"Prolog source-code for dialog"::
	source(D, Source),
	pretty_print(Source, String).


behaviour_model(D, Model:msg_model) :<-
	"Related behaviour model"::
	get(D, hypered, behaviour_model, Model).


open_behaviour_model(D) :->
	"Open the behaviour-model view"::
	(   get(D, behaviour_model, Model)
	->  send(Model, open),
	    send(Model?frame, expose)
	;   new(Model, msg_model_editor(D)),
	    send(Model, open)
	).


catch_all(D, Selector:name, Args:any ...) :->
	"Catch run-mode messages"::
	(   get(D, mode, run)
	->  send(@pce, send_vector, write_ln, 'Message', Selector, Args)
	;   fail			% editor error: report
	).


:- pce_end_class.
	

		 /*******************************
		 *	      SOURCE		*
		 *******************************/

:- pce_begin_class(dia_proto_icon, bitmap, "Icon for a prototype").

variable(prototype,	graphical,	get, "Represented prototype").

initialise(I, Proto:graphical) :->
	get(Proto, proto, Name),
	icon(Name, Image),
	send(I, send_super, initialise, Image),
	send(I, slot, prototype, Proto).

:- pce_global(@dia_proto_icon_recogniser,
	      make_dia_proto_icon_recogniser).

make_dia_proto_icon_recogniser(G) :-
	new(I, handler(area_enter,
		       message(@receiver, report, status,
			       @receiver?identify))),
	new(O, handler(area_exit,
		       message(@receiver, report, status, ''))),
	new(D, drag_and_drop_gesture(left, @default, @off)),
	send(D, get_source, @arg1?prototype),
	new(H, click_gesture(left, '', single,
			     message(@event?receiver, report, status,
				     'Use left button to drag object to dialog'))),
	new(G, handler_group(I, O, D, H)).

event(I, Ev:event) :->
	(   send(I, send_super, event, Ev)
	;   send(@dia_proto_icon_recogniser, event, Ev)
	).

identify(I, Str:string) :<-
	"Print identification string"::
	get(I?prototype, proto, Name),
	summary(Name, Text),
	Str = string(Text).

:- pce_end_class.


:- pce_begin_class(dia_source_dialog, dia_dialog, "Prototype dialog").

drop(D, DI:graphical, _Pos) :->
	(   (	get(DI, device, D)
	    ;	get(DI, device, @nil)
	    )
	->  true
	;   send(DI, free)		% delete
	).


preview_drop(D, DI:graphical*) :->
	send(@event, instance_of, event),
	get(@event, window, Window),
	(   DI == @nil
	->  (   get(D, attribute, saved_drag_cursor, Cursor)
	    ->	send(Window, focus_cursor, Cursor),
		send(D, delete_attribute, saved_drag_cursor)
	    ;	true
	    )
	;   (   (   get(DI, device, D)
		;	get(DI, device, @nil)
		)
	    ->  fail
	    ;   send(D, attribute, saved_drag_cursor, Window?focus_cursor),
		send(Window, focus_cursor, pirate)
	    )
	).


fill(D) :->
	"Append all prototypes"::
	forall(proto(Name, _, _, _),
	       send(D, append_prototype, Name)).

append_prototype(D, Name:name) :->
	"Append named prototype"::
	proto(Name, Term, Options, _Realise),
	Term =.. [Class|Args],
	make_proto_class(Class, ProtoClass),
	NewTerm =.. [ProtoClass|Args],
	new(Item, NewTerm),
	forall(member(Selector := Value, Options),
	       send(Item, Selector, Value)),
	send(Item, proto, Name),
	send(D, append, dia_proto_icon(Item)).

append(D, I:dia_proto_icon) :->
	"Append in 4 rows"::
	(   (	\+ get(D?graphicals, find,
		       message(@arg1, instance_of, dia_proto_icon), _)
	    ;	items_in_current_row(D, 9)
	    )
	->  send(D, send_super, append, I)
	;   send(D, send_super, append, I, right)
	).

items_in_current_row(D, N) :-
	get(D?graphicals, tail, Last),
	count_items(Last, N).

count_items(I, N) :-
	get(I, left, I2), I2 \== @nil, !,
	count_items(I2, N2),
	N is N2 + 1.
count_items(_I, 1).
	
:- pce_end_class.


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

:- pce_begin_class(dia_editor, frame, "Dialog Editor").

initialise(DE) :->
	dia_version(Version),
	send(DE, send_super, initialise,
	     string('Dialog Editor --- Version %s', Version)),
	send(DE, icon_label, 'Dialog Editor'),
	send(DE, append, new(TD, dialog)),
	send(new(B, browser), left, new(MD, dialog)),
	send(B, label, 'Dialog windows'),
	send(B, open_message,
	     and(message(DE?target, show, @on),
		 message(DE?target, expose))),
	send(B?list_browser, recogniser,
	     new(G, drag_and_drop_dict_item_gesture)),
	send(G, get_source, ?(DE, target, @arg1?key)),
	send(B, height, 5),
	send(B, below, TD),
	send(new(BD, dialog), below, B),
	send(BD, name, info_dialog),
	send(BD, gap, size(10,0)),
	send(BD, append, label(reporter, 'See file menu for help')),
	send(new(SD, dia_source_dialog), below, BD),
	fill_top_dialog(TD),
	fill_menu_dialog(MD),
	send(SD, fill),
	send(DE, mode, create).


report_to(DE, Reporter:graphical) :<-
	get(DE, member, top_dialog, Dialog),
	get(Dialog, member, reporter, Reporter).


fill_top_dialog(D) :-
	send(D, name, top_dialog),
	send(D, gap, size(5,5)),
	get(D, frame, Frame),
	new(Target, Frame?target),

	send(D, append, new(MB, menu_bar)),
	send(MB, append, new(File, popup(file))),
	send(MB, append, new(Open, popup(open_dialog))),

	send_list(File, append,
		  [ menu_item(help,
			      message(@helper, give_help, dialog,
				      'main:file')),
		    menu_item(bluffers_guide,
			      message(@helper, give_help, dialog,
				      bluffers_guide),
			      end_group := @on),
		    menu_item(new_dialog, message(Frame, create_target),
			      end_group := @on),
		    menu_item(reload_from_id, message(Frame, reload),
			      end_group := @on),
		    menu_item(load, message(Frame, load)),
		    menu_item(save, message(Frame, save),
			      condition := ?(Target,attribute,dia_save_file)),
		    menu_item(save_as, message(Frame, save_as),
			      condition := Target),
		    menu_item(save_all, message(Frame, save_all),
			      end_group := @on),
		    menu_item(postscript_as, message(Frame, postscript_as),
			      condition := Target,
			      end_group := @on),
		    menu_item(quit, message(Frame, destroy))
		  ]),

	NoTargets = not(message(Frame?targets, empty)),

	send_list(Open, append,
		  [ menu_item(open,
			      message(Target, show, @on),
			      condition := Target),
		    menu_item(open_all,
			      message(Frame, open_all_targets),
			      end_group := @on,
			      condition := NoTargets),
		    menu_item(close,
			      message(Target, show, @off),
			      condition := Target),
		    menu_item(close_all,
			      message(Frame, close_all_targets),
			      end_group := @on,
			      condition := NoTargets)
		  ]).


fill_menu_dialog(D) :-
	send(D, name, mode_dialog),
	get(D, frame, Frame),
	send(D, append, label(title, 'Mode', bold)),
	send(D, append,
	     new(M, menu(mode, marked, message(Frame, mode, @arg1)))),
	send(M, append, create),
	send(M, append, layout),
	send(M, append, action),
	send(M, append, run),
	send(M, show_label, @off),
	send(M, compute),
	send(M, reference, point(M?item_offset?x, 0)),
	send(D, append, new(Dev, device)),
	send(Dev, name, button_device),
	send(Dev, alignment, column).


mode(DE, Mode:{create,layout,action,run}) :->
	get(DE, member, mode_dialog, Dialog),
	get(Dialog, member, mode, Menu),
	get(Dialog, member, button_device, Dev),
	send(Menu, selection, Mode),
	send(Dev, clear),
	(   mode_button(Mode, _, _)
	->  forall(mode_button(Mode, Label, Message),
		   send(Dev, append_dialog_item,
			button(Label, Message)))
	;   send(Dev, display, graphical(0, 0, 1, button(x)?height))
	),
	send(Dev, layout_dialog),
	send(Dialog, layout),
	(   get(DE, target, Target)
	->  send(Target, mode, Mode)
	;   true
	).

:- pce_global(@dia_target, new(@receiver?frame?target)).

mode_button(layout, layout,      message(@dia_target, fix_layout)).
mode_button(layout, undo,	 message(@dia_target, restore_dialog_layout)).
mode_button(layout, fit,         message(@dia_target, fit_size)).
mode_button(action, behaviour_model,
			         message(@dia_target, open_behaviour_model)).
mode_button(run,    reset,       message(@dia_target, reset)).


create_target(DE) :->
	send(DE, mode, create),
	get(DE, member, browser, B),
	between(1, 10000, N),
	get(string('dialog_%s', N), value, DefName),
	\+ get(B, member, DefName, _), !,
	prompter('Create new dialog',
		 [ name:name  = Name/DefName
		 ]),
	new(Target, new(dia_target_dialog)),
	send(Target, name, Name),
	get(DE, area, Area),
	get(Area, top_side, Top),
	get(Area, right_side, Right),
	send(Target, open, point(Right+20, Top-30)),
	send(DE, target, Target).

target(DE, D:dia_target_dialog) :->
	get(D, name, Name),
	get(DE, member, browser, B),
	(   get(B, member, Name, DI)
	->  true
	;   send(B, append, new(DI, dict_item(Name)))
	),
	send(B, selection, DI),
	(   get(DE, target, Name, _)
	->  true
	;   new(_, dia_transient_hyper(DE, D, target, editor))
	).

deleted_target(DE, D:dia_target_dialog) :->
	"Update browser after target has been deleted"::
	(   send(DE, unlinking)
	->  true
	;   get(DE, member, browser, B),
	    get(D, name, Name),
	    send(B, delete, Name)
	).
		

target(DE, Name:[name], D:dia_target_dialog) :<-
	(   Name == @default
	->  get(DE, member, browser, B),
	    (	get(B, selection, DI), DI \== @nil
	    ->	get(DI, key, TheName)
	    ;	fail			% warning?
	    )
	;   TheName = Name
	),
	get(DE, hypered, target, @arg3?name == TheName, D).


targets(DE, Targets:chain) :<-
	"New chain holding the current targets"::
	get(DE, member, browser, B),
	new(Targets, chain),
	send(B?members, for_all,
	     message(Targets, append, ?(DE, target, @arg1?key))).


close_all_targets(DE) :->
	"->show: @off all <-targets"::
	send(DE?targets, for_all, message(@arg1, show, @off)).

open_all_targets(DE) :->
	"->show: @on all <-targets"::
	send(DE?targets, for_all, message(@arg1, show, @on)).


save_as(DE) :->
	get(DE, target, Target),
	get(Target, name, N0),
	get(N0, ensure_suffix, '.dia', N1),
	get(@finder, file, @off, '.dia', @default, N1, File),
	send(Target, attribute, attribute(dia_save_file, File)),
	send(DE, save).


save(DE) :->
	get(DE, target, Target),
	(   get(Target, attribute, dia_save_file, File)
	->  send(file(File), backup),
	    (	get(Target, find_hyper, behaviour_model, Hyper)
	    ->	send(Hyper, save_in_file, File)
	    ;	send(Target, save_in_file, File)
	    ),
	    send(DE, report, status, 'Saved in %s', file(File)?base_name)
	;   send(DE, save_as)		% ask file first
	).
	    

save_all(DE, File:[file]) :->
	"Save group of dialogs, so they stay together"::
	(   File == @default
	->  get(@finder, file, @off, '.dia', @default, TheFileName),
	    new(TheFile, file(TheFileName))
	;   TheFile = File
	),
	get(DE, member, browser, B),
	new(Ch, chain),
	send(B?dict?members, for_all, 
	     message(DE, add_target, Ch, @arg1?key)),
	send(Ch, save_in_file, TheFile),
	send(Ch, done),
	send(DE, report, status, 'Saved in %s', TheFile?base_name).
				      

add_target(DE, Ch:chain, TargetName:name) :->
	"Add target to chain (part of ->save_all"::
	get(DE, target, TargetName, Target),
	(   get(Target, find_hyper, behaviour_model, Hyper)
	->  send(Ch, append, Hyper)
	;   send(Ch, append, Target)
	).


load(DE) :->
	get(@finder, file, @on, '.dia', File),
	get(file(File), object, Loaded),
	rebind(DE, Loaded, File).


rebind(DE, Loaded, File) :-
	(   send(Loaded, instance_of, chain)
	->  send(Loaded, for_all,
		 message(@prolog, rebind, DE, @arg1, @default))
	;   (   send(Loaded, instance_of, hyper)
	    ->  get(Loaded, from, Target),
		send(Loaded?to, relink)
	    ;   send(Loaded, instance_of, dia_target_dialog)
	    ->  Target = Loaded
	    ),
	    (	File \== @default
	    ->	send(Target, dia_save_file, File)
	    ;	true
	    ),
	    send(DE, target, Target),
	    send(DE, mode, Target?(mode))
	).


reload(DE) :->
	"Import from source"::
	prompter('Import dialog from key',
		 [ key:name = Key
		 ]),
	load_dialog(Target, Key),
	send(Target, name, Key),
	get(DE, area, Area),
	get(Area, top_side, Top),
	get(Area, right_side, Right),
	send(Target, open, point(Right+20, Top-30)),
	send(DE, target, Target).


postscript_as(DE) :->
	"Dump <-target as PostScript"::
	get(DE, target, Target),
	(   get(Target, attribute, dia_postscript_file, PsFile)
	->  true
	;   get(Target, attribute, dia_save_file, SaveFile)
	->  get(SaveFile, delete_suffix, '.dia', Base),
	    get(Base, ensure_suffix, '.ps', PsFile)
	;   get(Target, name, Base),
	    get(Base, ensure_suffix, '.ps', PsFile)
	),
	get(@finder, file, @off, '.ps', @default, PsFile, ThePsFile),
	postscript(Target?frame, ThePsFile).

:- pce_end_class.
