
/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2001 University of Amsterdam. All rights reserved.
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


	
