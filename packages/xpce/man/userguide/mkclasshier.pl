/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Build PostScript version for the class-hierarchy holding all (built-in)
classes.  Called from the documentation Makefile.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

mkhierarchy(Out) :-
	new(T, tree(new(R, node(text(object))))),
	expand_node(R),
	send(T, compute),
	postscript(T, Out).

expand_node(N) :-
	get(N?string, value, Name),
	get(@pce, convert, Name, class, Class),
	get_chain(Class?sub_classes, map(@arg1?name), Subs), !,
	sort(Subs, Sorted),
	forall(member(C, Sorted),
	       (   send(N, son, new(S, node(text(C)))),
		   expand_node(S)
	       )).
expand_node(_).



