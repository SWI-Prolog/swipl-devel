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
