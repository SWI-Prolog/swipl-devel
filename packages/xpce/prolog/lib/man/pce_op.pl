/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1998 University of Amsterdam. All rights reserved.
*/

:- module(pce_by_operator,
	  [ (->>)/2
	  ]).
:- meta_predicate
	->>(+, :).

:- use_module(library(pce)).
:- require([ throw/1
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
	action(Obj, [Sel]).
	
action(A, _) :-
	var(A), !,
	throw(error(instantiation_error, (->>)/2)).
action(A = Obj, Sels) :- !,
	gets(Sels, Obj, A).
action(Obj->>Sel1, Sel) :- !,
	action(Obj, [Sel1|Sel]).
action(Obj, Sels) :- !,
	sends(Sels, Obj).

gets([Sel], Obj, A) :- !,
	get(Obj, Sel, A).
gets([S1|Sels], Obj, A) :-
	get(Obj, S1, O1),
	gets(Sels, O1, A).

sends([Sel], Obj) :- !,
	send(Obj, Sel).
sends([S1|Sels], Obj) :-
	get(Obj, S1, O1),
	sends(Sels, O1).


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
