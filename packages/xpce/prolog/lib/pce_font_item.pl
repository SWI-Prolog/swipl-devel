/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

:- module(pce_font_item, []).
:- use_module(library(pce)).
:- require([ send_list/2
	   , default/3
	   ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This file defines class font_item.   Class   font_item  is a dialog-item
specialised in entering font-values.  It consists   of three cycle menus
for the family, style and point-size of the font.  The interface is very
similar to the interface of  the   built-in  dialog-items  such as class
text_item and friends.  Summary:

	<->selection:	Access to current font
	<->default:	Default value, which may be function (->restore)
	->apply:	Execute the item

Though a bit complicated due to all  interaction between the items, this
class may be used as an   example/template  for defining compound dialog
items.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- pce_begin_class(font_item, device, "Dialog item for defining a font").

variable(message,	code*,		both,	"Message executed on change").
variable(default,	'font|function',get,	"Default value").
variable(value_set,	chain,		get,	"List of fonts").

initialise(FI, Name:[name],
	   Default:'[font|function]', Message:[code]*,
	   ValueSet:[chain]) :->
	"Create font-selector"::
	default(Name, font, Nm),
	default(Message, @nil, Msg),
	default(Default, font(screen, roman, 13), Def),
	send(FI, slot, message, Msg),
	send(FI, slot, default, Def),
	send(FI, send_super, initialise),
	send(FI, name, Nm),
	send(FI, alignment, column),
	send(FI, auto_label_align, @on),
	send(FI, append_dialog_item,
	     new(Fam, menu(family, cycle, message(FI, family, @arg1)))),
	send(FI, append_dialog_item,
	     new(Wgt, menu(weight, cycle, message(FI, weight, @arg1))), right),
	send(FI, append_dialog_item,
	     new(Pts, menu(points, cycle, message(FI, points, @arg1))), right),
	send(Fam, label, ?(Fam, label_name, Nm)),
	send(Wgt, label, ''),
	send(Pts, label, ''),
	send(FI, value_set, ValueSet),
	send(FI, default, Def),
	send(FI, layout_dialog, size(0, 0)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
The label of the compound  dialog-item   is  maintained by the left-most
menu.   Communication  on  <->label_width  ensures  that  the  label  is
properly aligned with other dialog-items above  and below this one.  See
also `dialog_item ->auto_label_align'.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

label(FI, Label:name) :->
	"Set the label"::
	get(FI, member, family, Fam),
	send(Fam, label, Label),
	send(FI, layout_dialog, size(0, 0)).

label_width(FI, W:int) :<-
	get(FI, member, family, Fam),
	get(Fam, label_width, W).
label_width(FI, W:int) :->
	get(FI, member, family, Fam),
	get(Fam, slot, label_width, O),
	send(Fam, label_width, W),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Wgt, relative_move, point(W-O, 0)),
	send(Pts, relative_move, point(W-O, 0)).

reference(FI, Ref:point) :<-
	"Dialog item reference point"::
	get(FI, member, family, Fam),
	get(Fam, reference, Ref).
	     

active(FI, Val:bool) :->
	send(FI?graphicals, for_all, message(@arg1, active, Val)).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Assign the item a value set (set of   fonts  to choose from).  This will
make entries in the three menus.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

value_set(FI, ValueSet:[chain]) :->
	"Define set of available fonts"::
	(   ValueSet == @default
	->  new(Set, chain),
	    send(@fonts, for_all, message(Set, append, @arg2))
	;   Set = ValueSet
	),
	send(FI, slot, value_set, Set),
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send_list([Fam, Wgt, Pts], clear),
	send(Set, for_all,
	     and(if(not(?(Fam, member, @arg1?family)),
		    message(Fam, append, @arg1?family)),
		 if(not(?(Wgt, member, @arg1?style)),
		    message(Wgt, append, @arg1?style)),
		 if(not(?(Pts, member, @arg1?points)),
		    message(Pts, append, @arg1?points)))),
	send(Fam, sort),
	send(Wgt, sort),
	send(Pts, sort, ?(@arg1?value, compare, @arg2?value)).


		 /*******************************
		 *	      CHANGES		*
		 *******************************/

forward(FI) :->
	(   send(FI?device, modified_item, FI, @on)
	->  true
	;   send(FI, apply)
	).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
pick_active(+Menu)  changes  the  selection  of  a  menu  to  an  active
menu-item close to the current one of the current one is not active.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

pick_active(M) :-
	get(M, active_item, M?selection, @on), !.
pick_active(M) :-
	get(M, selection, S),
	get(M, member, S, MI),
	get(M, members, Chain),
	(   get(Chain, find, @arg1?active == @on, _)
	->  get(Chain, index, MI, I),
	    pick_active(Chain, I, 0, MIA),
	    (   MIA == MI
	    ->  true
	    ;   send(M, selection, MIA)
	    )
	;   true
	).


pick_active(Chain, I, Offset, MI) :-
	(   Offset mod 2 =:= 1
	->  Idx is I + Offset//2
	;   Idx is I - Offset//2
	),
	get(Chain, nth1, Idx, MI),
	get(MI, active, @on), !.
pick_active(Chain, I, Offset, MI) :-
	NewOffset is Offset+1,
	pick_active(Chain, I, NewOffset, MI).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
After some menu has changed, this  method activates all styles available
to the current family  and  all   point-sizes  available  to the current
family/style combination.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

activate(FI) :->
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	get(FI, value_set, ValueSet),
	get(Fam, selection, CFam),
	new(Wgts, chain),
	new(Ptss, chain),
	send(ValueSet, for_all,
	     if(@arg1?family == CFam, message(Wgts, append, @arg1?style))),
	send(Wgt?members, for_all,
	     message(@arg1, active,
		     when(message(Wgts, member, @arg1?value), @on, @off))),
	pick_active(Wgt),
	get(Wgt, selection, CWgt),
	send(ValueSet, for_all,
	     if(and(@arg1?family == CFam,
		    @arg1?style == CWgt),
		message(Ptss, append, @arg1?points))),
	send(Pts?members, for_all,
	     message(@arg1, active,
		     when(message(Ptss, member, @arg1?value), @on, @off))),
	pick_active(Pts).


family(FI, _Fam:name) :->
	"User changed family cycle"::
	send(FI, activate),
	send(FI, forward).

weight(FI, _Wgt:name) :->
	send(FI, activate),
	send(FI, forward).

points(FI, _Pts:int) :->
	send(FI, forward).


		 /*******************************
		 *  GENERIC DIALOG OPERATIONS	*
		 *******************************/

selection(FI, Font:font) :->
	"Set the selection"::
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Fam, selection, Font?family),
	send(Wgt, selection, Font?style),
	send(Pts, selection, Font?points),
	send(FI, activate).

selection(FI, Font:font) :<-
	"Get the current font"::
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	get(Fam, selection, Family),
	get(Wgt, selection, Style),
	get(Pts, selection, Points),
	new(Font, font(Family, Style, Points)).


default(FI, Def:'font|function') :->
	send(FI, slot, default, Def),
	send(FI, restore).


restore(FI) :->
	get(FI, default, Def),
	send(FI, selection, Def).


apply(FI, _Always:[bool]) :->
	get(FI, message, Msg),
	(   Msg \== @nil
	->  send(Msg, forward, FI?selection)
	;   true
	).

:- pce_end_class.
