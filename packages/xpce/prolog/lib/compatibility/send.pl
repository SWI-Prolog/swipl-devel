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

