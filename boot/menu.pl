/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

:- module($win_menu,
	  [ win_insert_menu_item/4	% +PopupName, +Item, +Before, :Goal
	  ]).

:- module_transparent
	win_insert_menu_item/4.
:- multifile
	prolog:on_menu/1.
:- dynamic
	menu_action/2.
:- volatile
	menu_action/2.

prolog:on_menu(Label) :-
	menu_action(Label, Action),
	catch(Action, Error,
	      print_message(error, Error)).

%	win_insert_menu_item(+Popup, +Item, +Before, :Goal)
%
%	Add a menu-item to the PLWIN.EXE menu.  See the reference manual
%	for details.

win_insert_menu_item(Popup, --, Before, _Goal) :- !,
	$win_insert_menu_item(Popup, --, Before).
win_insert_menu_item(Popup, Item, Before, Goal) :-
	$strip_module(Goal, Module, PlainGoal),
	insert_menu_item(Popup, Item, Before, Module:PlainGoal).

insert_menu_item(Popup, Item, Before, Goal) :-
	(   menu_action(Item, OldGoal),
	    OldGoal \== Goal
	->  throw(error(permission_error(redefine, Item),
			win_insert_menu_item/4))
	;   true
	),
	$win_insert_menu_item(Popup, Item, Before),
	assert(menu_action(Item, Goal)).
