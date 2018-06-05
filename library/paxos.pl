/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald, Jan Wielemaker
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2009-2018, Jeffrey Rosenwald
                   CWI, Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(paxos,
          [ paxos_get/1,         % ?Term
            paxos_get/2,         % +Key, -Value
            paxos_get/3,         % +Key, -Value, +Options
            paxos_set/1,         % ?Term
            paxos_set/2,         % +Key, +Value
            paxos_set/3,         % +Key, +Value, +Options
            paxos_replicate/1,   % ?Term
            paxos_on_change/2,   % ?Term, +Goal
            paxos_on_change/3    % ?Key, ?Value, +Goal
          ]).
:- use_module(library(broadcast)).
:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(settings)).
:- use_module(library(option)).
:- use_module(library(error)).
:- use_module(library(when)).

/** <module> A Replicated Data Store

This module provides a replicated data store that is coordinated using a
variation on Lamport's Paxos concensus protocol.  The original method is
described in his paper entitled, "The   Part-time Parliament", which was
published in 1998. The algorithm is   tolerant of non-Byzantine failure.
That is late or lost delivery or   reply,  but not senseless delivery or
reply. The present algorithm takes advantage  of the convenience offered
by multicast to the quorum's membership,   who  can remain anonymous and
who can come and go as they  please without effecting Liveness or Safety
properties.

Paxos' quorum is a set of one or more attentive members, whose processes
respond to queries within some known time limit (< 20ms), which includes
roundtrip delivery delay. This property is   easy  to satisfy given that
every coordinator is necessarily a member of   the quorum as well, and a
quorum of one is  permitted.  An   inattentive  member  (e.g.  one whose
actions are late or lost) is deemed to be "not-present" for the purposes
of the present transaction and consistency   cannot  be assured for that
member. As long as there is at least one attentive member of the quorum,
then persistence of the database is assured.

Each member maintains a ledger  of   terms  along with information about
when  they  were   originally   recorded.    The   member's   ledger  is
deterministic. That is to say  that  there   can  only  be one entry per
functor/arity combination. No member will  accept   a  new term proposal
that has a line number that is equal-to   or  lower-than the one that is
already recorded in the ledger.

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

For practical reasons, we rely  on   the  partially synchronous behavior
(e.g. limited upper time bound for  replies) of broadcast_request/1 over
TIPC to ensure Progress. Perhaps more importantly,   we rely on the fact
that the TIPC broadcast listener state  machine guarantees the atomicity
of broadcast_request/1 at the process level, thus obviating the need for
external mutual exclusion mechanisms.

_|Note that this algorithm does not guarantee the rightness of the value
proposed. It only guarantees that if   successful, the value proposed is
identical for all attentive members of the quorum.|_

@author    Jeffrey Rosenwald (JeffRose@acm.org)
@license   BSD-2
@see       tipc_broadcast.pl, udp_broadcast.pl
*/

:- meta_predicate
    paxos_on_change(?, 0),
    paxos_on_change(?, ?, 0),
    basic_paxos_on_change(+, ?, ?, 0).

:- multifile
    paxos_message_hook/3.                       % +PaxOS, +TimeOut, -Message

:- setting(max_sets, nonneg, 20,
           "Max Retries to get to an agreement").
:- setting(max_gets, nonneg, 5,
           "Max Retries to get a value from the forum").
:- setting(response_timeout, float, 0.020,
           "Max time to wait for a response").


%!  c_element(+NewList, +Old, -Value)
%
%   A Muller c-element is a logic block  used in asynchronous logic. Its
%   output assumes the value of its  input   iff  all  of its inputs are
%   identical. Otherwise, the output retains its original value.

c_element([New | More], _Old, New) :-
    forall(member(N, More), N == New),
    !.
c_element(_List, Old, Old).

%!  paxos_initialize is det.
%
%   causes any required runtime initialization to occur. It is called as
%   a side-effect of initialize/0, which is now required as part of
%   an applications initialization directive.

paxos_initialize :-
    listening(paxos, _, _),
    !.
paxos_initialize :-
    listen(paxos, paxos(X), paxos_message(X)),
    basic_paxos_on_change(paxos, Key, Value, paxos_audit(Key, Value)).
%
% The Paxos state machine is memoryless. The state is managed by a
% coordinator.
%
paxos_audit(Key, Value) :-
    (   paxos_get(Key, Value)
    ->  true
    ;   paxos_set(Key, Value)
    ).

paxos_message(prepare(Key,K,Value)) :-
    (   ledger(Key, K, _)
    ->  true
    ;   K = 0,
        ledger_create(Key, K, Value)
    ),
    debug(paxos, 'Prepared ~p-~p@~d', [Key,Value,K]).
paxos_message(accept(Key,K,KA,Value)) :-
    debug(paxos, 'Accept ~p-~p@~p?', [Key, Value, K]),
    (   ledger_update(Key, K, Value)
    ->  debug(paxos, 'Accepted ~p-~p@~d', [Key,Value,K]),
        KA = K
    ;   debug(paxos, 'Rejected ~p@~d', [Key, K]),
        KA = nack
    ).
paxos_message(retrieve(Key,K,Value)) :-
    debug(paxos, 'Retrieving ~p', [Key]),
    ledger(Key,K,Value),
    debug(paxos, 'Retrieved ~p-~p@~d', [Key,Value,K]),
    !.

%%  paxos_set(+Term) is semidet.
%
%   Equivalent to paxos_key(Term,Key), pasox_set(Key,Term).   I.e., Term
%   is a ground compound term and its   key is the name/arity pair. This
%   version provides compatibility with older versions of this library.

%%  paxos_set(+Key, +Value) is semidet.
%%  paxos_set(+Key, +Value, +Options) is semidet.
%
%   negotiates to have Key-Value recorded in the  ledger for each of the
%   quorum's members. This predicate succeeds  if the quorum unanimously
%   accepts the proposed term. If no such   entry  exists in the Paxon's
%   ledger, then one is silently  created.   paxos_set/1  will retry the
%   transaction several times (default: 20)   before failing. Failure is
%   rare and is usually the result of a collision of two or more writers
%   writing to the same term at precisely  the same time. On failure, it
%   may be useful to wait some random period of time, and then retry the
%   transaction. By specifying a retry count   of zero, paxos_set/2 will
%   succeed iff the first ballot succeeds.
%
%   On   success,   paxos_set/1   will   also     broadcast   the   term
%   paxos(changed(Key,Value), to the quorum.
%
%   Options processed:
%
%     - retry(Retries)
%     is a non-negative integer specifying the number of retries that
%     will be performed before a set is abandoned.  Defaults to the
%     _setting_ `max_sets` (20).
%     - timeout(+Seconds)
%     Max time to wait for the forum to reply.  Defaults to the
%     _setting_ `response_timeout` (0.020, 20ms).
%
%   @arg Term is a compound  that   may  have  unbound variables.

paxos_set(Term) :-
    paxos_key(Term, Key),
    paxos_set(Key, Term, []).

paxos_set(Key, Value) :-
    paxos_set(Key, Value, []).

paxos_set(Key, Value, Options) :-
    must_be(ground, Key-Value),
    paxos_initialize,
    option(retry(Retries), Options, Retries),
    option(timeout(TMO), Options, TMO),
    apply_default(Retries, max_sets),
    apply_default(TMO, response_timeout),
    paxos_message(prepare(Key,R,Value), TMO, Prepare),
    between(0, Retries, _),
    findall(R, broadcast_request(Prepare), Rs),
    debug(paxos, 'Prepare: ~p', [Rs]),
    max_list(Rs, K),
    succ(K, K1),
    paxos_message(accept(Key,K1,R,Value), TMO, Accept),
    findall(R, broadcast_request(Accept), R1s),
    c_element(R1s, K, K1),
    paxos_message(changed(Key,Value), -, Changed),
    broadcast(Changed),
    !.
paxos_set(Key, Value, _) :-
    throw(error(paxos_error(set(Key, Value)), _)).

apply_default(Var, Setting) :-
    var(Var),
    !,
    setting(Setting, Var).
apply_default(_, _).

%!  paxos_get(?Term) is semidet.
%
%   Equivalent to paxos_key(Term,Key), pasox_get(Key,Term).   I.e., Term
%   is a compound term and its key  is the name/arity pair. This version
%   provides compatibility with older versions of this library.

%!  paxos_get(+Key, +Value) is semidet.
%!  paxos_get(+Key, +Value, +Options) is semidet.
%
%   unifies Term with the entry retrieved from the Paxon's ledger. If no
%   such entry exists in the member's local   cache,  then the quorum is
%   asked to provide a value,  which   is  verified  for consistency. An
%   implied paxos_set/1 follows. This predicate  succeeds if a term
%   with the same functor and arity exists   in  the Paxon's ledger, and
%   fails otherwise.
%
%   Options processed:
%
%     - retry(Retries)
%     is a non-negative integer specifying the number of retries that
%     will be performed before a set is abandoned.  Defaults to the
%     _setting_ `max_gets` (5).
%     - timeout(+Seconds)
%     Max time to wait for the forum to reply.  Defaults to the
%     _setting_ `response_timeout` (0.020, 20ms).
%
%   @arg Term is a compound. Any unbound variables are unified with
%   those provided in the ledger entry.

paxos_get(Term) :-
    paxos_key(Term, Key),
    paxos_get(Key, Term, []).
paxos_get(Key, Value) :-
    paxos_get(Key, Value, []).

paxos_get(Key, Value, _) :-
    ledger(Key, _Line, Value),
    !.
paxos_get(Key, Value, Options) :-
    paxos_initialize,
    option(retry(Retries), Options, Retries),
    option(timeout(TMO), Options, TMO),
    apply_default(Retries, max_gets),
    apply_default(TMO, response_timeout),
    Msg = Line-Value,
    paxos_message(retrieve(Key,Line,Value), TMO, Retrieve),
    between(0, Retries, _),
    findall(Msg, broadcast_request(Retrieve), Terms),
    c_element(Terms, no, Msg),
    paxos_set(Key, Value),
    !.

%!  paxos_key(+Term, -Key) is det.
%
%   Compatibility to allow for paxos_get/1, paxos_set/1, etc. The key of
%   a compound term is a term `'$c'(Name,Arity)`.   Note  that we do not
%   use `Name/Arity` and `X/Y` is  naturally   used  to organize keys as
%   hierachical _paths_.

paxos_key(Compound, '$c'(Name,Arity)) :-
    compound(Compound), !,
    compound_name_arity(Compound, Name, Arity).
paxos_key(Compound, _) :-
    must_be(compound, Compound).


%!  paxos_replicate(?Term) is det.
%
%   declares that Term is to be   automatically replicated to the quorum
%   each time it becomes grounded.  It   uses  the  behavior afforded by
%   when/2.
%
%   @arg Term is an ungrounded Term

paxos_replicate(X) :-
    when(ground(X), paxos_set(X)).

%!  paxos_on_change(?Term, :Goal) is det.
%!  paxos_on_change(?Key, ?Value, :Goal) is det.
%
%   executes the specified Goal  when   Key  changes.  paxos_on_change/2
%   listens for paxos(changed(Key,Value) notifications   for  Key, which
%   are emitted as the result   of  successful paxos_set/3 transactions.
%   When one is received for Key, then   Goal  is executed in a separate
%   thread of execution.
%
%   @arg Term is a compound, identical to that used for
%   paxos_get/1.
%   @arg Goal is one of:
%     - a callable atom or term, or
%     - the atom =ignore=, which causes monitoring for Term to be
%       discontinued.

paxos_on_change(Term, Goal) :-
    paxos_key(Term, Key),
    paxos_on_change(Key, Term, Goal).

paxos_on_change(Key, Value, Goal) :-
    Goal = _:Plain,
    (   Plain == ignore
    ->  unlisten(paxos_user, paxos(changed(Key,Value)))
    ;   basic_paxos_on_change(paxos_user, Key, Value, Goal)
    ).

basic_paxos_on_change(Owner, Key, Value, Goal) :-
    must_be(callable, Goal),
    paxos_initialize,
    listen(Owner, paxos(changed(Key,Value)),
           thread_create(Goal, _, [detached(true)])).

		 /*******************************
		 *    HOOK BROADCAST MESSAGES	*
		 *******************************/

%!  paxos_message(+PaxOS, +TimeOut, -BroadcastMessage) is det.
%
%   Transform a basic PaxOS message in   a  message for the broadcasting
%   service. This predicate is hooked   by paxos_message_hook/3 with the
%   same signature.
%
%   @arg TimeOut is one of `-` or a time in seconds.

paxos_message(Paxos, TMO, Message) :-
    paxos_message_hook(paxos(Paxos), TMO, Message),
    !.
paxos_message(Paxos, TMO, Message) :-
    throw(error(mode_error(det, fail,
                           paxos:paxos_message_hook(Paxos, TMO, Message)), _)).


		 /*******************************
		 *         HOOK STORAGE		*
		 *******************************/

:- dynamic
    paxons_ledger/3.

%!  ledger(+Key, -Gen, -Value) is semidet.
%
%   True if the ledger has Value associated with Key at generation Gen.

ledger(Key, Gen, Value) :-
    paxons_ledger(Key, Gen, Value).

%!  ledger_create(+Key, +Gen, +Value) is det.
%
%   Create a new Key-Value pair at generation Gen.

ledger_create(Key, Gen, Value) :-
    asserta(paxons_ledger(Key, Gen, Value)).

%!  ledger_update(+Key, +Gen, +Value) is semidet.
%
%   Update Key to Value if the current generation is older than Gen.

ledger_update(Key, Gen, Value) :-
    clause(paxons_ledger(Key, Gen0, _Value), true, Ref),
    Gen > Gen0,
    asserta(paxons_ledger(Key, Gen, Value)),
    erase(Ref).
