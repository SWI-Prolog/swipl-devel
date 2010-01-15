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

:- module(tipc_paxos,
	  [
	   tipc_paxos_get/1,         % ?Term
	   tipc_paxos_set/1,         % ?Term
	   tipc_paxos_set/2,         % ?Term,  +Retries
	   tipc_paxos_replicate/1,   % ?Term
	   tipc_paxos_on_change/2,   % ?Term,  +Goal
	   tipc_initialize/0
	  ]).

:- use_module(tipc_broadcast).

/** <module> A Replicated Data Store
This module provides a replicated data store that is coordinated using a
variation on Lamport's Paxos concensus protocol. The original method is
described in his paper entitled, "The   Part-time Parliament", which was
published in 1998. The algorithm is   tolerant of non-Byzantine failure.
That is late or lost delivery or   reply,  but not senseless delivery or
reply. The present algorithm takes advantage of the convenience offered
by multicast to the quorum's membership, who can remain anonymous and
who can come and go as they please without effecting Liveness or
Safety properties.

Paxos' quorum is a set of one or more attentive members, whose
processes respond to queries within some known time limit (< 20ms),
which includes roundtrip delivery delay. This property is easy to
satisfy given that every coordinator is necessarily a member of the
quorum as well, and a quorum of one is permitted. An inattentive member
(e.g. one whose actions are late or lost) is deemed to be "not-present"
for the purposes of the present transaction and consistency
cannot be assured for that member. As long as there is at least one
attentive member of the quorum, then persistence of the database is
assured.

Each member maintains a ledger of terms along with information about
when they were originally recorded. The member's ledger is
deterministic. That is to say that there can only be one entry per
functor/arity combination. No member will accept a new term proposal
that has a line number that is equal-to or lower-than
the one that is already recorded in the ledger.

Paxos is a three-phase protocol:

   1: A coordinator first prepares the quorum for a new proposal by
   broadcasting a proposed term. The quorum responds by returning the
   last known line number for that functor/arity combination that is
   recorded in their respective ledgers.

   2: The coordinator selects the highest line number it receives,
   increments it by one, and then asks the quorum to finally accept the
   new term with the new line number. The quorum checks their respective
   ledgers once again and if there is still no other ledger entry for
   that functor/arity combination that is equal-to or higher than the
   specified line, then each member records the term in the ledger at
   the specified line. The member indicates consent by returning the
   specified line number back to the coordinator. If consent is withheld
   by a member, then the member returns a =nack= instead. The
   coordinator requires unanimous consent. If it isn't achieved then the
   proposal fails and the coordinator must start over from the
   beginning.

   3: Finally, the coordinator concludes the successful negotiation by
   broadcasting the agreement to the quorum in the form of a
   =|paxos_changed(Term)|= event. This is the only event that should be
   of interest to user programs.

For practical reasons, we rely on the partially synchronous behavior
(e.g. limited upper time bound for replies) of broadcast_request/1 over
TIPC to ensure Progress. Perhaps more importantly, we rely on the fact
that the TIPC broadcast listener state machine guarantees the atomicity
of broadcast_request/1 at the process level, thus obviating the need for
external mutual exclusion mechanisms.

_|Note that this algorithm does not guarantee the rightness of the value
proposed. It only guarantees that if successful, the value proposed is
identical for all attentive members of the quorum.|_

_|Note also that tipc_paxos now requires an initialization step. See
tipc_initialize/0.|_

@author    Jeffrey Rosenwald (JeffRose@acm.org)
@license   LGPL
@see       tipc_broadcast.pl
@compat    Linux only, tipc_broadcast
*/

% %
% A Muller c-element is a logic block used in asynchronous logic. Its
% output assumes the value of its input iff all of its inputs are
% identical. Otherwise, the output retains its original value.
%

c_element([New | More], _Old, New) :-
	forall(member(N, More), N == New), !.

c_element(_List, Old, Old).

% tipc_paxos_initialize is det.
% causes any required runtime initialization to occur. It is called as
% a side-effect of tipc_initialize/0, which is now required as part of
% an applications initialization directive.
%

tipc_paxos_initialize :-
	listening(tipc_paxos, _, _), !.

tipc_paxos_initialize :-
	listen(tipc_paxos, X, tipc_paxos_message(X)),
	tipc_basic_paxos_on_change(tipc_paxos, Term, tipc_paxos_audit(Term)).
%
% The Paxos state machine is memoryless. The state is managed by a
% coordinator.
%
tipc_paxos_audit(Term) :-
	tipc_paxos_get(Term) ->
	     true;
	     tipc_paxos_set(Term).

tipc_paxos_message(paxos_prepare(K-Term)) :-
	recorded(Term, paxons_ledger(K, _Term)),
	!.

tipc_paxos_message(paxos_prepare(0-Term)) :-
	recorda(Term, paxons_ledger(0, Term)).

tipc_paxos_message(paxos_accept(K-Term, K)) :-
	recorded(Term, paxons_ledger(K1, _Term), Ref),
	K > K1,
	recorda(Term, paxons_ledger(K, Term)),
	erase(Ref),
	!.

tipc_paxos_message(paxos_accept(_, nack)).

tipc_paxos_message(paxos_retrieve(K-Term)) :-
	recorded(Term, paxons_ledger(K, Term)),
	!.

%
% These are the cooordinator predicates
%
max_sets(20).    % retry N times before failing
max_gets(5).

%%  tipc_paxos_set(?Term) is semidet.
%%  tipc_paxos_set(?Term, +Retries) is semidet.
%     negotiates to have Term recorded in  the   ledger  for  each of the
% quorum's members. This predicate  succeeds   if  the quorum unanimously
% accepts the proposed term. If  no  such   entry  exists  in the Paxon's
% ledger, then one is silently created.   tipc_paxos_set/1 will retry the
% transaction several times (default: 20) before failing. Failure is rare
% and is usually the result of a collision of two or more writers writing
% to the same term at precisely the  same   time.  On  failure, it may be
% useful to wait  some  random  period  of   time,  and  then  retry  the
% transaction. By specifying a retry count of zero, tipc_paxos_set/2 will
% succeed iff the first ballot succeeds.
%
% On success, tipc_paxos_set/1 will also broadcast the term
% =|paxos_changed(Term)|=, to the quorum.
%
% @param Term is a compound  that   may  have  unbound variables.
% @param Retries (optional) is a non-negative integer  specifying the
% number of retries that will be performed before a set is abandoned.
%
tipc_paxos_set(Term) :-
	max_sets(N),
	tipc_paxos_set(Term, N),
	!.

tipc_paxos_set(Term, Retries) :-
	compound(Term),
	between(0, Retries, _),
	findall(R, broadcast_request(tipc_cluster(paxos_prepare(R-Term), 0.020)), Rs),
	max_list(Rs, K),
	succ(K, K1),
	findall(R, broadcast_request(tipc_cluster(paxos_accept(K1-Term, R), 0.020)), R1s),
	c_element(R1s, K, K1),
	broadcast(tipc_cluster(paxos_changed(Term))),
	!.

%% tipc_paxos_get(?Term) is semidet.
% unifies Term with the entry   retrieved  from the Paxon's
% ledger. If no such entry exists in   the member's local cache, then the
% quorum is asked to provide a value,  which is verified for consistency.
% An implied tipc_paxos_set/1 follows. This predicate succeeds if a term with
% the same functor and arity exists in the Paxon's ledger, and fails
% otherwise.
%
% @param Term is a compound.  Any   unbound  variables  are unified with
% those provided in the ledger entry.
%

tipc_paxos_get(Term) :-
	recorded(Term, paxons_ledger(_K, Term)),
	!.

tipc_paxos_get(Term) :-
	max_gets(N),
	between(1,N, _),
	findall(K-Term, broadcast_request(tipc_cluster(paxos_retrieve(K-Term), 0.020)), Terms),
	c_element(Terms, no, K-Term),
	tipc_paxos_set(Term),
	!.

%% tipc_paxos_replicate(?Term) is det.
% declares that Term is to be automatically replicated to the quorum
% each time it becomes grounded. It uses the behavior afforded by
% when/2.
%
% @param Term is an ungrounded Term
%

tipc_paxos_replicate(X) :-
	when(ground(X), tipc_paxos_set(X)).

%% tipc_paxos_on_change(?Term, :Goal) is det.
% executes the specified Goal when Term changes. tipc_paxos_on_change/2
% listens for paxos_changed/1 notifications for Term, which are emitted
% as the result of successful tipc_paxos_set/1 transactions. When one is
% received for Term, then Goal is executed in a separate thread of
% execution.
%
%  @param Term is a compound, identical to that used for
%  tipc_paxos_get/1.
%  @param Goal is one of:
%   * a callable atom or term, or
%   * the atom =ignore=, which causes monitoring for Term to be
%   discontinued.
%
:- meta_predicate
	tipc_paxos_on_change(?, :).

tipc_paxos_on_change(Term, Goal) :-
	must_be(compound, Term),
	Goal = _:Plain,
	(   Plain == ignore
	->  unlisten(tipc_paxos_user, paxos_changed(Term))
	;   tipc_basic_paxos_on_change(tipc_paxos_user, Term, Goal)
	).

% Private
tipc_basic_paxos_on_change(Owner, Term, Goal) :-
	callable(Goal),
	listen(Owner, paxos_changed(Term),
	       thread_create(Goal, _, [detached(true)])).

%%	tipc_initialize is semidet.
%   See tipc:tipc_initialize/0.
%

:- multifile tipc:tipc_stack_initialize/0.

%   tipc_stack_initialize is det. called as a side-effect of
%   tipc:tipc_initialize/0.
%
tipc:tipc_stack_initialize :-
      tipc_paxos_initialize, !.
