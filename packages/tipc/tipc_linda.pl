/*  Part of SWI-Prolog

    Author:        Jeffrey Rosenwald
    E-mail:        jeffrose@acm.org
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2010, Jeffrey Rosenwald

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

:- module(tipc_linda,
	  [
	   linda/0,                  %
	   linda/1,                  % +Term
	   linda_client/1,           % +Address
	   close_client/0,           %
	   linda_timeout/1,	     % +Number
	   linda_timeout/2,          % ?Number, ?Number
	   out/1,                    % +Term
	   in/1,		     % ?Term
	   in_noblock/1,             % ?Term
	   in/2,                     % +List, ?Term
	   rd/1,                     % ?Term
	   rd_noblock/1,             % ?Term
	   rd/2,                     % +List, ?Term
	   bagof_rd_noblock/3,       % +Template, ?Term, -Bag
	   bagof_in_noblock/3,	     % +Template, ?Term, -Bag
	   linda_eval/1,             % :Head
	   linda_eval/2,	     % ?Head, :Body
	   linda_eval_detached/1,    % :Head
	   linda_eval_detached/2,    % ?Head, :Body
	   tuple/1,                  % :Goal
	   tuple/2,		     % ?Head, :Body
	   tipc_linda_server/0,	     %
	   tipc_initialize/0
	  ]).

:- use_module(library(tipc/tipc_broadcast)).
:- use_module(library(unix)).

:- require([ broadcast/1
	   , broadcast_request/1
	   , throw/1
	   , member/2
	   , must_be/2
	   , catch/3
	   , flag/3
	   , listen/3
	   , setup_call_cleanup/3
	   , strip_module/3
	   ]).


/** <module> A Process Communication Interface

Linda is a framework for building systems  that are composed of programs
that cooperate among themselves in order  to   realize  a larger goal. A
Linda application is  composed  of  two   or  more  processes  acting in
concert. One process acts as a  server   and  the others act as clients.
Fine-grained communications between client and server is provided by way
of message passing over sockets and   support  networks, TIPC sockets in
this case. Clients interact indirectly by way  of the server. The server
is in principle an eraseable blackboard that   clients  can use to write
(out/1), read (rd/1) and remove (in/1)  messages called _|tuples.|_ Some
predicates will fail if  a  requested  tuple   is  not  present  on  the
blackboard. Others will block until a  tuple instance becomes available.
Tuple instances are made available to  clients   by  writing them on the
blackboard using out/1.

In TIPC Linda, there is a  subtle   difference  between the =in= and the
=rd= predicates that is  worth  noting.   The  =in=  predicates  succeed
exactly once for each tuple placed  in   the  tuple  space. The tuple is
provided to exactly one  requesting  client.   Clients  can  contend for
tuples in this way, thus  enabling   multi-server  operations.  The =rd=
predicates succeed nondeterministically, providing   all matching tuples
in the tuple space at a given time to the requesting client as a choice
point without disturbing them.

TIPC Linda is inspired by and adapted   from the SICStus Prolog API. But
unlike SICStus TCP Linda, TIPC  Linda   is  connectionless.  There is no
specific session between client and  server.   The  server  receives and
responds to datagrams originated by clients in an epiperiodic manner.

Example: A simple producer-consumer.

In client 1:
==
init_producer :-
       linda_client(global),
       producer.

producer :-
       produce(X),
       out(p(X)),
       producer.

produce(X) :- .....
==
In client 2:
==
init_consumer :-
	linda_client(global),
	consumer.

consumer :-
       in(p(A)),
       consume(A),
       consumer.

consume(A) :- .....
==

Example: Synchronization
==
       ...,
       in(ready),  %Waits here until someone does out(ready)
       ...,
==
Example: A critical region
==
       ...,
       in(region_free),  % wait for region to be free
       critical_part,
       out(region_free), % let next one in
       ...,
==
Example: Reading global data
==
       ...,
       rd(data(Data)),
       ...,
==
or, without blocking:
==
       ...,
       (rd_noblock(data(Data)) ->
             do_something(Data)
       ;     write('Data not available!'),nl
       ),
       ...,
==

Example: Waiting for any one of several events
==
       ...,
       in([e(1),e(2),...,e(n)], E),
%  Here is E instantiated to the first tuple that became available
       ...,
==

Example: Producers and Consumers in the same process using =linda_eval=
threads and/or =tuple= predicates

==
  consumer1 :-
	repeat,
	in([p(_), quit], Y),
	(   Y = p(Z) -> writeln(consuming(Z)); !),
	fail.

  producer1 :-
	forall(between(1,40, X), out(p(X))).

  producer_consumer1 :-
	linda_eval(consumer1),
	call_cleanup(producer1, out(quit)), !.
%
%
  consumer2 :-
       between(1,4,_),
       in_noblock(p(X)), !,
       writeln(consuming(X)),
       consumer2.

  producer2 :-
	linda_eval(p(X), between(1,40, X)).

  producer_consumer2 :-
	producer2,
	linda_eval(consumer2), !.
%
%
  consumer3 :-
	forall(rd_noblock(p(X)), writeln(consuming(X))).

  producer3 :-
	tuple(p(X), between(1,40, X)).

  producer_consumer3 :-
	producer3,
	linda_eval(done, consumer3),
	in(done), !.
==

---++  Servers

   The server is the process running the "blackboard process". It is
   part of TIPC Linda. It is a collection of predicates that are
   registered as tipc_broadcast listeners. The server process can be run
   on a separate machine if necessary.

   To load the package, enter the query:
   ==
   ?- use_module(library(tipc/tipc_linda)).

   ?- linda.
      TIPC Linda server now listening at: port_id('<1.1.1:3200515722>')
      true.
   ==

---++ Clients

   The clients are one or more Prolog processes that have connection(s)
   to the server.

   To load the package, enter the query:
   ==
   ?- use_module(library(tipc/tipc_linda)).

   ?- linda_client(global).
      TIPC Linda server listening at: port_id('<1.1.1:3200515722>')
      true.
   ==

@see Nicholas Carriero and David Gelernter. _|How to Write Parallel
Programs: A First Course.|_ The MIT Press, Cambridge, MA, 1990.

@author Jeffrey A. Rosenwald

@compat SWI-Prolog for Linux only
@compat tipc_broadcast library
*/

:- meta_predicate eventually_implies(0,0), ~>(0,0), safely(0).

safely(Goal) :-
	catch(Goal, Err, (print_message(error, Err), fail)).

eventually_implies(P, Q) :-
	setup_call_cleanup(P, (Foo = true; Foo = false), assertion(Q)),
	Foo == true.

:- op(950, xfy, ~>).

~>(P, Q) :- eventually_implies(P, Q).


:- dynamic(linda_data/1).

%
%    This is the backend state machine
%

linda_action(rd(listening)) :- !.

linda_action(in(TupleList, Tuple)) :-
	member(Tuple, TupleList),
	retract(linda_data(Tuple)), !.

linda_action(in(Tuple)) :-
	retract(linda_data(Tuple)), !.

linda_action(out(Tuple)) :-
	assert(linda_data(Tuple)).

linda_action(rd(TupleList, Tuple)) :-
	member(Tuple, TupleList),
	linda_data(Tuple).

linda_action(rd(Tuple)) :-
	linda_data(Tuple).

linda_action(bagof_rd_noblock(Template,	Var^Tuple, Bag)) :-
	!, bagof(Template, Var^linda_data(Tuple), Bag).

linda_action(bagof_rd_noblock(Template, Tuple, Bag)) :-
	!, bagof(Template, linda_data(Tuple), Bag).

linda_action(bagof_in_noblock(Template,	Var^Tuple, Bag)) :-
	Datum = linda_data(Tuple),
	!, bagof(Template, Var^(Datum, retract(Datum)), Bag).

linda_action(bagof_in_noblock(Template, Tuple, Bag)) :-
	!, bagof(Template, retract(linda_data(Tuple)), Bag).

%
%    This is the user interface
%

%%	linda is det.
%%	linda(:Goal) is det.
%   Starts a Linda-server in this process. The
%   network address is written to current output stream as a TIPC
%   port_id/2 reference (e.g. port_id('<1.1.1:3200515722>') ). This
%   predicates looks to see if a server is already listening on the
%   cluster. If so, it reports the address of the existing server.
%   Otherwise, it registers a new server and reports its address.
%
% ==
% ?- linda.
%    TIPC Linda server now listening at: port_id('<1.1.1:3200515722>')
%    true.
%
% ?- linda.
%    TIPC Linda server still listening at: port_id('<1.1.1:3200515722>')
%    true.
% ==
%
%  The following will call my_init/0 in the current module after the
%  server is successfully started or is found already listening.
%  my_init/0 could start client-processes, initialize the tuple space,
%  etc.
%
% ==
% ?- linda(my_init).
% ==
%

linda_listening(Addr) :-
	basic_request(rd(listening), Addr), !.

linda :-
	linda_listening(Addr), !,
	format('TIPC Linda server still listening at: ~p~n', [Addr]).

linda :-
	listen(tipc_linda, '$linda'(Action), linda_action(Action)),
	linda_listening(Addr), !,
	format('TIPC Linda server now listening at: ~p~n', [Addr]).

:- meta_predicate linda(0).

linda(Hook) :-
	linda,
	call(Hook).

%%	linda_client(+Domain) is semidet.
%
%    Establishes a connection to a Linda-server  providing a named tuple
%    space. Domain is  an  atom   specifying  a  particular tuple-space,
%    selected from a universe of tuple-spaces.  At present however, only
%    one tuple-space, =global=, is supported. A client may interact with
%    any server reachable on the TIPC  cluster. This predicate will fail
%    if no server is reachable for that tuple space.
%

linda_client(global) :-
	linda_listening(Addr), !,
	format('TIPC Linda server listening at: ~p~n', [Addr]).

%%	close_client is det.
%
% Closes the connection to  the  Linda-server.   Causes  the  server  to
% release resources associated with this client.

close_client :- true.   % Presently a noop

%%	linda_timeout(?OldTime, ?NewTime) is semidet.
%
% Controls Linda's message-passing timeout. It specifies the time window
% where clients will accept server replies in  response to =in= and =rd=
% requests.  Replies  arriving  outside  of  this  window  are  silently
% ignored. OldTime is unified with the old   timeout and then timeout is
% set to NewTime.  NewTime  is  of   the  form  Seconds:Milliseconds.  A
% non-negative real number, seconds, is also  recognized. The default is
% 0.250 seconds. This timeout is thread local and is _not_  inherited
% from its parent. New threads are initialized to the default.
%
% *|Note:|* The synchronous behavior  afforded by in/1 and rd/1
% is implemented by periodically polling the   server.  The poll rate is
% set according to this timeout. Setting the timeout too small may
% result in substantial network traffic that is of little value.
%
% @throws error(feature_not_supported). SICStus Linda can
% disable the timeout by specifying =off= as NewTime. This feature does
% not exist for safety reasons.
%

:- thread_local linda_time_out/1.

linda_timeout(Time, Time) :-
	linda_time_out(Time), !.

linda_timeout(_OldTime, NewTime) :-
	NewTime == off,
	throw(error(feature_not_supported)).

linda_timeout(OldTime, NewTime) :-
	ground(NewTime),
	NewTime = Seconds:Milliseconds,
	NewTime1 is float(Seconds + (Milliseconds / 1000.0)),
	linda_timeout(OldTime, NewTime1), !.

linda_timeout(OldTime, NewTime) :-
	ground(NewTime),
	NewTime >= 0.020,
	clause(linda_time_out(OldTime), true, Ref),
	asserta(linda_time_out(NewTime)) -> erase(Ref), !.

linda_timeout(0.250, NewTime) :-
	NewTime >= 0.020,
	asserta(linda_time_out(NewTime)).


%%	linda_timeout(+NewTime) is semidet.
%
% Temporarily sets Linda's timeout. Internally,  the original timeout is
% saved and then the timeout is set  to NewTime. NewTime is as described
% in linda_timeout/2. The original timeout  is restored automatically on
% cut of choice points, failure on backtracking, or uncaught exception.
%

linda_timeout(NewTime) :-
	linda_timeout(OldTime, NewTime) ~>
	    linda_timeout(NewTime, OldTime).

basic_request(Action) :-
	basic_request(Action, _Addr).

basic_request(Action, Addr) :-
	linda_timeout(Time, Time),
	broadcast_request(tipc_cluster('$linda'(Action):Addr, Time)).

%%	out(+Tuple) is det.
%
%    Places a Tuple in Linda's tuple-space.
%

out(Tuple) :-
	broadcast(tipc_cluster('$linda'(out(Tuple)))), !.

%%	in(?Tuple) is det.
%
%    Atomically removes the tuple Tuple from   Linda's tuple-space if it
%    is there. The tuple will be returned   to exactly one requestor. If
%    no tuple is available, the predicate   blocks until it is available
%    (that is, someone performs an out/1).

in(Tuple) :-
	repeat,
	in_noblock(Tuple), !.

%%	in_noblock(?Tuple) is semidet.
%
%    Atomically removes the tuple Tuple from   Linda's tuple-space if it
%    is there. If not, the predicate fails.  This predicate can fail due
%    to a timeout.

in_noblock(Tuple) :-
	basic_request(in(Tuple)), !.

%%	in(+TupleList, -Tuple) is det.
%
%    As in/1 but succeeds when any  one   of  the tuples in TupleList is
%    available. Tuple is unified with the fetched tuple.

in(TupleList, Tuple) :-
	must_be(list, TupleList),
	repeat,
	basic_request(in(TupleList, Tuple)), !.

%%	rd(?Tuple) is nondet.
%
%    Succeeds  nondeterministically  if  Tuple  is    available  in  the
%    tuple-space, suspends otherwise until it is available. Compare this
%    with in/1: the tuple is not removed.

rd(Tuple) :-
	repeat,
	rd_noblock(Tuple).

%%	rd_noblock(?Tuple) is nondet.
%
%    Succeeds  nondeterministically  if  Tuple  is    available  in  the
%    tuple-space, fails otherwise. This predicate  can   fail  due  to a
%    timeout.

rd_noblock(Tuple) :-
	basic_request(rd(Tuple)).

%%	rd(?TupleList, -Tuple) is nondet.
%
%    As in/2 but provides a  choice  point   that  does  not  remove any
%    tuples.

rd(TupleList, Tuple) :-
	must_be(list, TupleList),
	repeat,
	basic_request(rd(TupleList, Tuple)).

%%	bagof_in_noblock(?Template, ?Tuple, -Bag) is nondet.
%%	bagof_rd_noblock(?Template, ?Tuple, -Bag) is nondet.
%
%    Bag is the list of all instances of Template such that Tuple exists
%    in the tuple-space. The behavior of variables in Tuple and Template
%    is as in bagof/3. The variables   could be existentially quantified
%    with ^/2 as in bagof/3. The  operation   is  performed as an atomic
%    operation. This predicate can  fail  due   to  a  timeout. Example:
%    Assume that only one client is connected to the server and that the
%    tuple-space initially is empty.
%
%  ==
%    ?- out(x(a,3)), out(x(a,4)), out(x(b,3)), out(x(c,3)).
%
%    true.
%    ?- bagof_rd_noblock(C-N, x(C,N), L).
%
%    L = [a-3,a-4,b-3,c-3] .
%
%    true.
%    ?- bagof_rd_noblock(C, N^x(C,N), L).
%
%    L = [a,a,b,c] .
%
%    true.
%  ==

bagof_rd_noblock(Template,  Tuple, Bag) :-
	!, basic_request(bagof_rd_noblock(Template, Tuple, Bag)).

bagof_in_noblock(Template,  Tuple, Bag) :-
	!, basic_request(bagof_in_noblock(Template, Tuple, Bag)).

:- meta_predicate
      linda_eval(?, 0),
      linda_eval(0),
      linda_eval_detached(?, 0),
      linda_eval_detached(0).

%%	linda_eval(:Goal) is det.
%%	linda_eval(?Head, :Goal) is det.
%%	linda_eval_detached(:Goal) is det.
%%	linda_eval_detached(?Head, :Goal) is det.
%
%  Causes Goal to be evaluated in parallel  with a parent predicate. The
%  child  thread  is  a  full-fledged    client,   possessing  the  same
%  capabilities as the  parent.  Upon   successful  completion  of Goal,
%  unbound variables are unified and the  result   is  sent to the Linda
%  server via out/1, where  it  is   made  available  to  others. linda_eval/2
%  evaluates Goal, then unifies the result  with Head, providing a means
%  of customizing the resulting output structure.   In linda_eval/1, Head, and
%  Goal are identical, except that the module  name for Head is stripped
%  before output. If the child fails  or receives an uncaught exception,
%  no such output occurs.
%
%  *|Joining Threads:|* Threads created using linda_eval/(1-2) are not allowed
%  to linger. They are joined (blocking  the parent, if necessary) under
%  three conditions: backtracking on failure into an linda_eval/(1-2), receipt
%  of an uncaught  exception,  and  cut   of  choice-points.  Goals  are
%  evaluated   using   forall/2.   They   are    expected   to   provide
%  nondeterministic behavior. That is they  may   succeed  zero  or more
%  times on backtracking. They must however,  eventually fail or succeed
%  deterministically.  Otherwise,  the  thread  will  hang,  which  will
%  eventually hang the parent  thread.  Cutting   choice  points  in the
%  parent's body has the effect of joining   all children created by the
%  parent. This provides a  barrier  that   guarantees  that  all  child
%  instances of Goal have run to completion before the parent proceeds.
%  Detached threads behave as above, except that they operate
%  independently and cannot be joined. They will continue to run while
%  the host process continues to run.
%
% Here is an example of a parallel quicksort:
%
% ==
% qksort([], []).
%
% qksort([X | List], Sorted) :-
%	partition(@>(X), List, Less, More),
%	linda_eval(qksort(More, SortedMore)),
%	qksort(Less, SortedLess), !,
%	in_noblock(qksort(More, SortedMore)),
%	append(SortedLess, [X | SortedMore], Sorted).
% ==
%
linda_eval(Head) :-
	linda_eval(Head, Head).

linda_eval(Head, Body) :-
	must_be(callable, Body),
	strip_module(Head, _Module, Plain),
	thread_create(forall(Body, out(Plain)), Id, []) ~>
	   thread_join(Id, true).

linda_eval_detached(Head) :-
	linda_eval_detached(Head, Head).

linda_eval_detached(Head, Body) :-
	must_be(callable, Body),
	strip_module(Head, _Module, Plain),
	thread_create(forall(Body, out(Plain)), _Id, [detached(true)]).

%%	tuple(:Goal) is det.
%%      tuple(?Head, :Goal) is det.
%
%  registers Head as a virtual tuple  in   TIPC  Linda's tuple space. On
%  success, any client on the  cluster   may  reference the tuple, Head,
%  using rd/1 or rd_noblock/1.  On  reference,   Goal  is  executed by a
%  separate thread of execution in the host client's Prolog process. The
%  result is unified with Head, which  is   then  returned  to the guest
%  client. As in linda_eval/(1-2) above, Goal is evaluated using forall/2. The
%  virtual tuple is unregistered  on   backtracking  into a tuple/(1-2),
%  receipt of uncaught exception, or cut   of choice-points. In tuple/1,
%  Head and Goal are identical, except that  the module name is stripped
%  from Head.
%
%  *|Note:|* A virtual tuple is an extension  of the server. Even though
%  it is operating in the client's  Prolog environment, it is restricted
%  in the server operations that it may   perform.  It is generally safe
%  for tuple predicates to perform out/1   operations,  but it is unsafe
%  for them to perform any variant of   =in= or =rd=, either directly or
%  indirectly. This restriction is however, relaxed   if  the server and
%  client are operating in separate  heavyweight processes (not threads)
%  on the node or cluster. This is   most  easily achieved by starting a
%  stand-alone   Linda   server   somewhere   on    the   cluster.   See
%  tipc_linda_server/0, below.
%
:- meta_predicate tuple(?, 0), tuple(0).

tuple(Head) :-
	tuple(Head, Head).

tuple(Head, Body) :-
	must_be(callable, Body),
	strip_module(Head, _Module, Plain),
	listen(user, '$linda'(rd(Plain)), Body) ~>
	    unlisten(user, '$linda'(rd(Plain)), Body).

%% tipc_linda_server is nondet.
%
%   Acts as a stand-alone Linda server.   This predicate initializes the
%   TIPC stack and then starts a Linda  server in the current thread. If
%   a client performs  an  =|out(server_quit)|=,   the  server's  Prolog
%   process will exit via halt/1. It is intended for use in scripting as
%   follows:
%
%   ==
%   swipl -q -g 'use_module(library(tipc/tipc_linda)),
%          tipc_linda_server' -t 'halt(1)'
%   ==
%
%   See also manual section 2.10.2.1 Using PrologScript.
%
%   *|Note:|*  Prolog  will  return  a  non-zero  exit  status  if  this
%   predicate is executed on  a  cluster   that  already  has  an active
%   server. An exit status of zero is returned on graceful shutdown.
%
%   @throws error(permission_error(halt,thread,2),context(halt/1,Only
%   from thread 'main')), if this predicate is executed in a thread
%   other than =main=.
%
%
wait_for_quit :-
	linda_timeout(6.0),
	in(server_quit),
	halt(0).

tipc_linda_server :-
%	detach_IO,               % become a daemon
	tipc_initialize,
	(   linda_client(global) -> true; linda(wait_for_quit)).

%%	tipc_initialize is semidet.
%
%   See tipc:tipc_initialize/0.
%

