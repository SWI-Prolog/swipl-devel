/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

:- module(broadcast,
	  [ listen/3,		% Listener x Templ x Goal
	    listen/2,		% Templ x Goal
	    unlisten/1,		% Listener
	    unlisten/2,		% Listener x Templ
	    unlisten/3,		% Listener x Templ x Goal
	    listening/3,	% Listener x Templ x Goal
	    broadcast/1,	% Templ
	    broadcast_request/1	% Templ
	  ]).
:- meta_predicate
	listen(+, :),
	listen(+, +, :),
	unlisten(+, +, :).

:- use_module(library(pce)).		% actually, only for require/1
:- require([ strip_module/3
	   ]).

:- dynamic
	listener/4.

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Generic broadcasting service. Broadcasts are   made  using the predicate
broadcast(+Templ). All registered  `listeners'  will   have  their  goal
called. Success or failure of this is ignored. The listener can not bind
arguments.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%	listen([+Listener], +Templ, +Goal)
%
%	Open a channel for listening for events of the given `Templ'.

listen(Listener0, Templ, Goal) :-
	canonical_listener(Listener0, Listener),
	strip_module(Goal, Module, TheGoal),
	assert_listener(Templ, Listener, Module, TheGoal).

listen(Templ, Goal) :-
	strip_module(Goal, Module, TheGoal),
	assert_listener(Templ, Module, Module, TheGoal).


%	unlisten(+Listener, [+Templ, [+Goal]])
%
%	Destroy a channel.  Arguments may be variables

unlisten(Listener0) :-
	canonical_listener(Listener0, Listener),
	retractall(listener(_, Listener, _, _)).
unlisten(Listener0, Templ) :-
	canonical_listener(Listener0, Listener),
	retractall(listener(Templ, Listener, _, _)).
unlisten(Listener0, Templ, Goal) :-
	canonical_listener(Listener0, Listener),
	(   var(Goal)
	->  true
	;   strip_module(Goal, Module, TheGoal)
	),
	retract_listener(Templ, Listener, Module, TheGoal).


%	listening(-Listener, -Templ, -Goal)
%
%	returns currently open channels

listening(Listener0, Templ, Module:Goal) :-
	canonical_listener(Listener0, Listener),
	listener(Templ, Listener, Module, Goal).


%	broadcast(+Templ, +Arg)
%
%	Broadcast given event.  Always succeeds.

broadcast(Templ) :-
	(   listener(Templ, _Listener, Module, Goal),
	    (   Module:Goal
	    ->  fail
	    )
	;   true
	).


%	broadcast_request(+Templ)
%
%	Broadcast given event till accepted.  Succeeds then, fail if no
%	listener accepts the call.  Bindings made by the listener goal
%	are maintained.  May be used to make broadcast requests.  Can
%	backtrack.

broadcast_request(Templ) :-
	listener(Templ, _Listener, Module, Goal),
	Module:Goal.


%	{assert,retract}_listener(+Templ, +Listener, +Module, +Goal)
%	
%	Implemented as sub-predicate to ensure storage in this module.
%	Second registration is ignored.  Is this ok?  It avoids problems
%	using multiple registration of global listen channels.

assert_listener(Templ, Listener, Module, TheGoal) :-
	listener(Templ, Listener, Module, TheGoal), !.
assert_listener(Templ, Listener, Module, TheGoal) :-
	asserta(listener(Templ, Listener, Module, TheGoal)).

retract_listener(Templ, Listener, Module, TheGoal) :-
	retractall(listener(Templ, Listener, Module, TheGoal)).

%	canonical_listener(+Raw, -Canonical)
%
%	Entry for later optimization.

canonical_listener(Templ, Templ).





