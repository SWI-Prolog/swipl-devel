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

:- module(pce_make_dialog,
	  [ make_dialog/2
	  ]).
:- meta_predicate make_dialog(-, :).

:- use_module(library(pce)).
:- require([ forall/2
	   , member/2
	   , memberchk/2
	   , send_list/3
	   , strip_module/3
	   ]).


make_dialog(Dialog, Id) :-
	strip_module(Id, Module, TheId),
	make_dialog(Dialog, Module, TheId).

make_dialog(Dialog, Module, TheId) :-
	Module:dialog(TheId, Attributes),
	memberchk(object := Dialog, Attributes),
	do(make_dialog_item,   parts,         Attributes),
	do(modify,             modifications, Attributes),
	do(popups,	       popups,	      Attributes),
	do(behaviour(Module),  behaviour,     Attributes),
	do(layout(Dialog),     layout,        Attributes),
	do(initialise,	       initialise,    Attributes).

do(Goal, Attribute, List) :-
	memberchk(Attribute := Value, List), !,
	maplist(Goal, Value).
do(_, _, _).


		 /*******************************
		 *	      PARTS		*
		 *******************************/

make_dialog_item(Var := NewTerm) :-
	new(Var, NewTerm).

		 /*******************************
		 *	  MODIFICATIONS		*
		 *******************************/

modify(Ref := List) :-
	modify(List, Ref).

modify([], _).
modify([Attr := Value|T], Ref) :-
	send_list(Ref, Attr, Value),
	modify(T, Ref).


		 /*******************************
		 *	      POPUPS		*
		 *******************************/

popups(Ref := [ PopupSelector := NewTerm, Attributes ]) :-
	new(Popup, NewTerm),
	modify(Popup := Attributes),
	send(Ref, PopupSelector, Popup).


		 /*******************************
		 *	      LAYOUT		*
		 *******************************/

layout(Dialog, below(I1, I2)) :- !,
	attach(Dialog, I1, I2),
	send(I1, below, I2).
layout(Dialog, right(I1, I2)) :- !,
	attach(Dialog, I1, I2),
	send(I1, right, I2).
layout(Dialog, position(I1, Pos)) :-
	send(Dialog, display, I1, Pos).
layout(Dialog, area(I1, area(X,Y,W,H))) :-
	send(I1, auto_align, @off),
	send(I1, do_set, X, Y, W, H),
	send(Dialog, display, I1).

attach(Dialog, I1, _I2) :-
	get(I1, device, Dialog), !.
attach(Dialog, _I1, I2) :-
	get(I2, device, Dialog), !.
attach(Dialog, _I1, I2) :-
	send(Dialog, append, I2).


		 /*******************************
		 *	      DYNAMICS		*
		 *******************************/

behaviour(Module, Ref := List) :-
	forall(member(Attr := Value, List), Module:send(Ref, Attr, Value)).


		 /*******************************
		 *	     INITIALISE		*
		 *******************************/

initialise(_Name := Code) :- !,		% compatibility
	send(Code, forward).
initialise(Goal) :-
	Goal.
