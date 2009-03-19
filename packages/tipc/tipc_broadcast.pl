/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2008, Jeffrey Rosenwald

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(tipc_broadcast,
             [
		% there are no publicly exposed predicates
	     ]).

/** <module> A TIPC Broadcast Bridge

SWI-Prolog's broadcast library provides a  means   that  may  be used to
facilitate publish and subscribe communication regimes between anonymous
members of a community of interest.  The   members  of the community are
however, necessarily limited to a single   instance  of Prolog. The TIPC
broadcast library removes that restriction.   With  this library loaded,
any member of a TIPC network that also  has this library loaded may hear
and respond to your broadcasts.

This module has no public predicates. When it initialized, it did three
things:

    * It started  a  listener  daemon   thread  that  is  listening for
    broadcasts from others, received as TIPC datagrams, and

    * It registered three listeners: tipc_node/1, tipc_cluster/1, and
    tipc_zone/1, and

    * It registered three listeners: tipc_node/2, tipc_cluster/2, and
    tipc_zone/2.

A broadcast/1 or broadcast_request/1 that is not  directed to one of the
six listeners above, behaves as usual and is confined to the instance of
Prolog that originated it. But when so   directed, the broadcast will be
sent to all participating systems, including   itself,  by way of TIPC's
multicast  addressing  facility.  The  principal  functors  =tipc_node=,
=tipc_cluster=, and =tipc_zone=, specify the scope of the broadcast. The
functor =tipc_node=, specifies that the broadcast   is to be confined to
members  of  a  present   TIPC    node.   Likewise,  =tipc_cluster=  and
=tipc_zone=, specify that the traffic should  be confined to members of
a present TIPC cluster and zone,  respectively. To prevent the potential
for feedback loops, the scope  qualifier   is  stripped from the message
before transmission. See library module tipc.pl, for more information.

An example of three separate processes cooperating on the same Node:

==
Process A:

   ?- listen(number(X), between(1, 5, X)).
   true.

   ?-

Process B:

   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Process C:

   ?- findall(X, broadcast_request(tipc_node(number(X))), Xs).
   Xs = [1, 2, 3, 4, 5, 7, 8, 9].

   ?-
==

It is also  possible  to  carry  on   a  private  dialog  with  a single
responder. To do this, you supply a   compound of the form, Term:PortId,
to a TIPC scoped broadcast/1 or broadcast_request/1, where PortId is the
port-id of the intended listener.  If   you  supply an unbound variable,
PortId, to broadcast_request, it will be unified with the address of the
listener that responds to Term. You may   send a directed broadcast to a
specific  member  by  simply  providing  this  address  in  a  similarly
structured compound to a TIPC scoped   broadcast/1.  The message is sent
via unicast to that  member  only  by   way  of  the  member's broadcast
listener. It is received by the  listener   just  as any other broadcast
would be. The listener does not know the difference.

Although this capability is needed under   some  circumstances, it has a
tendency to compromise the resilience of the broadcast model. You should
not rely on it too heavily, or fault tolerance will suffer.

For example, in order to discover who responded with a particular value:

==
Process A:

   ?- listen(number(X), between(1, 3, X)).
   true.

   ?-

Process B:

   ?- listen(number(X), between(7, 9, X)).
   true.

   ?-

Process C:

   ?- broadcast_request(tipc_node(number(X):From)).
   X = 7,
   From = port_id('<1.1.1:3971170279>') ;
   X = 8,
   From = port_id('<1.1.1:3971170279>') ;
   X = 9,
   From = port_id('<1.1.1:3971170279>') ;
   X = 1,
   From = port_id('<1.1.1:3971170280>') ;
   X = 2,
   From = port_id('<1.1.1:3971170280>') ;
   X = 3,
   From = port_id('<1.1.1:3971170280>') ;
   false.

?-

==

---++ Caveats:

While the implementation is mostly transparent, there are some important
and subtle differences that must be taken into consideration:

    * Prolog's broadcast_request/1 is nondet. It sends the request,
    then evaluates the replies synchronously, backtracking as needed
    until a satisfactory reply is received. The remaining potential
    replies are not evaluated. This is not so when TIPC is involved.

    * A TIPC broadcast/1 is completely asynchronous.

    * A  TIPC broadcast_request/1 is partially synchronous. A
    broadcast_request/1 is sent, then the sender balks for a period of
    time (default: 250 ms) while the replies are collected. Any reply
    that is received after this period is silently discarded. An
    optional second argument is provided so that a sender may specify
    more (or less) time for replies.

    * Replies are collected using findall/3, then the list of replies
    is presented to the user as a choice-point, using member/2. If a
    listener is connected to a generator that always succeeds (e.g. a
    random number generator), then the broadcast request will never
    terminate and trouble is bound to ensue.

    * broadcast_request/1 with TIPC scope is _not_ reentrant (at
    least, not now anyway). If a listener performs a broadcast_request/1
    with TIPC scope recursively, then disaster looms certain. This
    caveat does not apply to a TIPC scoped broadcast/1, which can safely
    be performed from a listener context.

    * TIPC's capacity is not infinite. While TIPC can tolerate
    substantial bursts of activity, it is
    designed for short bursts of small messages. It can tolerate
    several thousand replies in response to a broadcast_request/1
    without trouble, but it will begin to encounter congestion beyond
    that. And in congested conditions, things will start to become
    unreliable as TIPC begins prioritizing and/or discarding traffic.

    * A TIPC broadcast_request/1 term that is grounded is considered to
    be a broadcast only. No replies are collected unless the there is at
    least one unbound variable to unify.

    * A TIPC broadcast/1 always succeeds, even if there are no
    listeners.

    * A TIPC broadcast_request/1 that receives no replies will fail.

    * Replies may be coming from many different places in the network
    (or none at all). No ordering of replies is implied.

    * Prolog terms are sent to others after first converting them to
    atoms using term_to_atom/2. Passing real numbers this way may
    result in a substantial truncation of precision. See prolog flag
    option, 'float_format', of current_prolog_flag/2.

@author    Jeffrey Rosenwald (JeffRose@acm.org)
@license   LGPL
@see       tipc.pl
@compat    Linux only
*/

:- use_module(tipc).
:- use_module(library(broadcast)).
:- use_module(library(time)).
:- use_module(library(unix)).

:- require([ once/1
	   , forall/2
	   , member/2
	   , term_to_atom/2
	   , thread_property/2
	   , catch/3
	   , setup_call_cleanup/3
	   , thread_create/3
	   ]).


tipc_broadcast_service(node,            name_seq(20005, 0, 0)).
tipc_broadcast_service(cluster,         name_seq(20005, 1, 1)).
tipc_broadcast_service(zone,            name_seq(20005, 2, 2)).

%
%  Here's a TIPC bridge to Prolog's broadcast library
%
%  A sender may confine a broadcast to  a   subset  of a TIPC network by
%  specifying a scoping qualifier in   his/her  broadcast. The qualifier
%  has the effect of selecting the   appropriate  multi-cast address for
%  the transmission. Thus, the sender of   the  message has control over
%  the scope of his/her traffic on a per-message basis.
%
%  All in-scope listeners receive the   broadcast and simply rebroadcast
%  the message locally. All broadcast replies, if any, are sent directly
%  to the sender via the port-id that   was received with the broadcast.
%  No additional multiplexing is required.
%

%%  try_finally(?Setup, +Cleanup) is multi.
%  Succeeds nondeterministically if Setup succeeds.
%  It executes Cleanup under one of three conditions:
%     * backtracking on failure into try_finally/2,
%     * An uncaught exception is thrown subsequent to
%     Setup, but before choice-points are cut, or
%     * Cut (!) of choice-points.
%
%     try_finally/2 is used to protect a   fixed  resource (e.g. socket,
%     mutex, stream, etc.) against leakage  under adverse circumstances.
%     Any variables that are bound in   Setup  are available to Cleanup,
%     and remain in scope going forward.  The   cut  is used to indicate
%     that the fixed resource is no longer needed and may be released.
%     It is released at the time of the cut, so any further usage of
%     the (formerly) allocated resource is likely to cause you
%     considerable trouble.
%

try_finally(Setup, Cleanup) :-
	setup_call_cleanup(Setup, (true; fail), Cleanup).

ld_dispatch(S, '$tipc_request'(wru(Name)), From) :-
	tipc_get_name(S, Name),
	term_to_atom(wru(Name), Atom),
	tipc_send(S, Atom, From, []).

ld_dispatch(S, '$tipc_request'(Term), From) :-
	forall(catch(broadcast_request(Term), _, fail),
	           (   term_to_atom(Term, Atom),
		       tipc_send(S, Atom, From, [])
	           )),
	!.

ld_dispatch(_S, Term, _From) :-
	catch(broadcast(Term),_, fail).

tipc_listener_daemon :-
	try_finally(tipc_socket(S, rdm), tipc_close_socket(S)),

	tipc_setopt(S, importance(medium)),
	tipc_setopt(S, dest_droppable(true)),  % discard if not deliverable

	forall(tipc_broadcast_service(Scope, Address),
	     tipc_bind(S, Address, scope(Scope))),

	repeat,
	     tipc_receive(S, Data, From, [as(atom)]),
	     term_to_atom(Term, Data),
	     once(ld_dispatch(S, Term, From)),
	fail.


start_tipc_listener_daemon :-
	thread_property(Id, alias(tipc_listener_daemon)),
	thread_property(Id, status(running)), !.

start_tipc_listener_daemon :-
	thread_create(tipc_listener_daemon, _,
	       [alias(tipc_listener_daemon), detached(true)]),

	listen(tipc_broadcast, Head, broadcast_listener(Head)).
%
broadcast_listener(tipc_broadcast_service(Class, Addr)) :-
	tipc_broadcast_service(Class, Addr).

broadcast_listener(tipc_node(X)) :-
	tipc_broadcast(X, node, 0.250).

broadcast_listener(tipc_cluster(X)) :-
	tipc_broadcast(X, cluster, 0.250).

broadcast_listener(tipc_zone(X)) :-
	tipc_broadcast(X, zone, 0.250).

broadcast_listener(tipc_node(X, Timeout)) :-
	tipc_broadcast(X, node, Timeout).

broadcast_listener(tipc_cluster(X, Timeout)) :-
	tipc_broadcast(X, cluster, Timeout).

broadcast_listener(tipc_zone(X, Timeout)) :-
	tipc_broadcast(X, zone, Timeout).

%
%

tipc_basic_broadcast(S, Term, Address) :-
	try_finally(tipc_socket(S, rdm), tipc_close_socket(S)),
	tipc_setopt(S, importance(medium)),
	term_to_atom(Term, Atom),
	tipc_send(S, Atom, Address, []).

% directed broadcast to a single listener
tipc_broadcast(Term:To, _Scope, _Timeout) :-
	ground(Term), ground(To), !,
	tipc_basic_broadcast(_S, Term, To),
	!.

% broadcast to all listeners
tipc_broadcast(Term, Scope, _Timeout) :-
	ground(Term), !,
	tipc_broadcast_service(Scope, Address),
	tipc_basic_broadcast(_S, Term, Address),
	!.

% directed broadcast_request to a single listener
tipc_broadcast(Term:Address, _Scope, Timeout) :-
	ground(Address), !,
        tipc_basic_broadcast(S, '$tipc_request'(Term), Address),
	tipc_br_collect_replies(S, Timeout, Replies),
	!, member([Term, Address], Replies).

% broadcast_request to all listeners returning responder port-id
tipc_broadcast(Term:From, Scope, Timeout) :-
	!, tipc_broadcast_service(Scope, Address),
        tipc_basic_broadcast(S, '$tipc_request'(Term), Address),
	tipc_br_collect_replies(S, Timeout, Replies),
	!, member([Term, From], Replies).

% broadcast_request to all listeners ignoring responder port-id
tipc_broadcast(Term, Scope, Timeout) :-
	tipc_broadcast(Term:_, Scope, Timeout).

tipc_br_send_timeout(Port) :-
	try_finally(tipc_socket(S, rdm), tipc_close_socket(S)),
	tipc_setopt(S, importance(critical)),
	tipc_send(S, '$tipc_br_timeout', Port, []),
	!.

tipc_br_collect_replies(S, Timeout, Terms) :-
	tipc_get_name(S, Port),
	try_finally(alarm(Timeout, tipc_br_send_timeout(Port), Id),
		    remove_alarm(Id)),

	findall([Term, From],
		(
		repeat,
	        tipc_receive(S, Atom, From, [as(atom)]),
                (   (Atom \== '$tipc_br_timeout')
		-> term_to_atom(Term, Atom)
		;  (!, fail)
		)
	        ), Terms).

:- initialization
     start_tipc_listener_daemon.


