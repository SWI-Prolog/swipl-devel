/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_make_dialog,
	  [ make_dialog/2
	  ]).
:- meta_predicate make_dialog(-, :).

:- use_module(library(pce)).
:- require([ checklist/2
	   , forall/2
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
	checklist(Goal, Value).
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
