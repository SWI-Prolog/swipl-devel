/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

:- module(pce_by_operator,
	  [ (->>)/2
	  ]).
:- meta_predicate
	->>(+, :).

:- use_module(library(pce)).
:- require([ throw/1
	   , strip_module/3
	   ]).

:- multifile
	user:goal_expansion/2.

:- op(800, yfx, user:(->>)).		% send/get
:- op(800, xfx, user:(*>>)).		% send/get super
:- op(800, xfx, user:(=>>)).		% send/get slot


		 /*******************************
		 *	      SEND/GET		*
		 *******************************/

%	TBD: make this a goal-expansion too.

Obj->>Sel :-
	strip_module(Sel, M, Msg),
	action(Obj, [Msg], M).
	
action(A, _, _) :-
	var(A), !,
	throw(error(instantiation_error, (->>)/2)).
action(A = Obj, Sels, M) :- !,
	gets(Sels, Obj, A, M).
action(Obj->>Sel1, Sel, M) :- !,
	action(Obj, [Sel1|Sel], M).
action(Obj, Sels, M) :- !,
	sends(Sels, Obj, M).

gets([Sel], Obj, A, M) :- !,
	get(Obj, M:Sel, A).
gets([S1|Sels], Obj, A, M) :-
	get(Obj, M:S1, O1),
	gets(Sels, O1, A, M).

sends([Sel], Obj, M) :- !,
	send(Obj, M:Sel).
sends([S1|Sels], Obj, M) :-
	get(Obj, M:S1, O1),
	sends(Sels, O1, M).


		 /*******************************
		 *	  SEND/GET-SUPER	*
		 *******************************/

expand(Rec*>>Msg, Expanded) :- !,
	(   nonvar(Rec),
	    Rec = (A = Obj)
	->  Expanded = get_super(Obj, Msg, A)
	;   Expanded = send_super(Rec, Msg)
	).

		 /*******************************
		 *	    SLOT ACCESS		*
		 *******************************/

expand(Rec=>>Msg, Expanded) :- !,
	(   nonvar(Rec),
	    Rec = (A = Obj)
	->  Expanded = get(Obj, slot(Msg, A))
	;   Msg =.. List,
	    EMsg =.. [slot|List],
	    Expanded = send(Rec, EMsg)
	).

pce_ifhostproperty(prolog(sicstus),
[(   :- multifile(user:goal_expansion/3)		),
 (   user:goal_expansion(G, M, E) :-
	M \== pce_by_operator,
	expand(G, E)
 )
],
[(   :- multifile(user:goal_expansion/2)		),
 (   user:goal_expansion(G, E) :- expand(G, E)		)
]).
