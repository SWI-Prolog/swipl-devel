/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/


:- module(pce_event_hierarchy,
	  [ event_hierarchy/0
	  ]).

:- use_module(library(pce)).
:- require([ chain_list/2
	   , forall/2
	   , member/2
	   ]).

event_hierarchy :-
	new(P, picture('PCE Event Hierarchy')),
	new(D, dialog),
	send(D, below, P),
	send(D, append, button(quit, message(P, destroy))),
	get(@event_tree, root, Root),
	new(T, tree(new(RootNode, node(text(Root?value, left, normal))))),
	fill_event_hierarchy(Root, RootNode),
	send(P, display, T),
	send(P, open).


fill_event_hierarchy(Node, TreeNode) :-
	get(Node, sons, Sons),
	Sons \== @nil, !,
	chain_list(Sons, List),
	forall(member(S, List),
	       (send(TreeNode, son, new(N, node(text(S?value, left, normal)))),
	        fill_event_hierarchy(S, N))).
fill_event_hierarchy(_, _).
