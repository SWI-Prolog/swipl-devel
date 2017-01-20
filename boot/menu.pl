/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2002-2011, University of Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module('$win_menu',
          [ win_insert_menu_item/4,     % +PopupName, +Item, +Before, :Goal
            win_has_menu/0              % Test whether we have menus
          ]).

:- meta_predicate
    win_insert_menu_item(+,+,+,:).
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

%       win_has_menu
%
%       Test whether the system provides the menu interface

win_has_menu :-
    current_predicate(_, '$win_insert_menu_item'(_, _, _)).

%       win_insert_menu_item(+Popup, +Item, +Before, :Goal)
%
%       Add a menu-item to the PLWIN.EXE menu.  See the reference manual
%       for details.

win_insert_menu_item(Popup, --, Before, _Goal) :-
    !,
    call('$win_insert_menu_item'(Popup, --, Before)). % fool check/0
win_insert_menu_item(Popup, Item, Before, Goal) :-
    insert_menu_item(Popup, Item, Before, Goal).

insert_menu_item(Popup, Item, Before, Goal) :-
    (   menu_action(Item, OldGoal),
        OldGoal \== Goal
    ->  throw(error(permission_error(redefine, Item),
                    win_insert_menu_item/4))
    ;   true
    ),
    call('$win_insert_menu_item'(Popup, Item, Before)),
    assert(menu_action(Item, Goal)).
