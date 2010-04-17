/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org/packages/xpce/
    Copyright (C): 2010, University of Amsterdam

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

:- module(pce_font_item, []).
:- use_module(library(pce)).
:- require([ default/3
	   , send_list/2
	   ]).

:- pce_begin_class(font_item, device).

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
	send(FI, append_dialog_item, new(Fam, menu(family, cycle))),
	send(FI, append_dialog_item, new(Wgt, menu(weight, cycle)), right),
	send(FI, append_dialog_item, new(Pts, menu(points, cycle)), right),
	send(Fam, label, ?(Fam, label_name, Nm)),
	send(Wgt, label, ''),
	send(Pts, label, ''),
	send(FI, value_set, ValueSet),
	send(FI, default, Def),
	send(FI, layout_dialog, size(0, 0)).


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
	get(Fam, label_width, O),
	send(Fam, label_width, W),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Wgt, relative_move, point(W-O, 0)),
	send(Pts, relative_move, point(W-O, 0)).


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
		 *	GENERIC DIALOG-ITEM	*
		 *******************************/

selection(FI, Font:font) :->
	"Set the selection"::
	get(FI, member, family, Fam),
	get(FI, member, weight, Wgt),
	get(FI, member, points, Pts),
	send(Fam, selection, Font?family),
	send(Wgt, selection, Font?style),
	send(Pts, selection, Font?points).

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
