/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
This module redefines send/[2,3] such  that they accepts lists, as was
the case upto version 4.3.4 of  xpce/swi-prolog.  This module can only
be loaded on top  of SWI-Prolog as it  uses some rather dirty  tricks.
Portable    code  should  use   the new   send_list/2  and send_list/3
predicates.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- abolish(send, 2).
:- abolish(send, 3).

:- export(pce_principal:'$pce_send'(_,_,_)).
:- import(pce_principal:'$pce_send'(_,_,_)).

:- module_transparent
	send/2,
	send/3.

:- index(send(1, 1)).
:- index(send(1, 1, 1)).

%	send_list(+ListOfObjs, +ListOfSels, [+Arg])
%
%	Send a messages to the carthesian product of ListOfObjs and
%	ListOfSels.

send([], _) :- !.
send(_, []) :- !.
send([Object|Objects], Selectors) :- !, 
	send(Object, Selectors), 
	send(Objects, Selectors).
send(Object, [Selector|Selectors]) :- !, 
	send(Object, Selector), 
	send(Object, Selectors).
send(Object, Selector) :-
	'$pce_send'(Object, Selector, arguments).


send([], _,  _) :- !.
send(_, [], _) :- !.
send(_, _, []) :- !.
send([Object|Objects], Selectors, Arguments) :- !, 
	send(Object, Selectors, Arguments), 
	send(Objects, Selectors, Arguments).
send(Objects, [Selector|Selectors], Arguments) :- !, 
	send(Objects, Selector, Arguments), 
	send(Objects, Selectors, Arguments).
send(Object, Selector, [Argument|Arguments]) :- !, 
	send(Object, Selector, Argument), 
	send(Object, Selector, Arguments).
send(Object, Selector, Argument) :-
	'$pce_send'(Object, Selector, arguments(Argument)).

