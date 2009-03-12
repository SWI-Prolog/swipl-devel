/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

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


:- module(http_session,
	  [ http_set_session_options/1,	% +Options

	    http_session_id/1,		% -SessionId
	    http_in_session/1,		% -SessionId
	    http_current_session/2,	% ?SessionId, ?Data

	    http_session_asserta/1,	% +Data
	    http_session_assert/1,	% +Data
	    http_session_retract/1,	% ?Data
	    http_session_retractall/1,	% +Data
	    http_session_data/1		% ?Data
	  ]).
:- use_module(http_wrapper).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(socket)).
:- use_module(library(broadcast)).

/** <module> HTTP Session management

This library defines session management based   on HTTP cookies. Session
management is enabled simply by  loading   this  module.  Details can be
modified using http_set_session_options/1.

If sessions are enabled, http_session_id/1  produces the current session
and http_session_assert/1 and friends maintain   data about the session.
If the session is reclaimed, all associated data is reclaimed too.

Begin and end of sessions can be monitored using library(broadcast). The
broadcasted messages are:

    * http_session(begin(SessionID, Peer))
    * http_session(end(SessionId, Peer))

I.e. the following  calls  end_session(SessionId)   whenever  a  session
terminates. Please note that sessions ends are not scheduled. Creating a
new session scans the  active  list   for  timed-out  sessions. This may
change in future versions of this library.

    ==
    :- listen(http_session(end(SessionId, Peer)),
	      end_session(SessionId)).
    ==
*/

:- dynamic
	session_setting/1,		% Name(Value)
	current_session/2,		% SessionId, Peer
	last_used/2,			% SessionId, Time
	session_data/2.			% SessionId, Data

session_setting(timeout(600)).		% timeout in seconds
session_setting(cookie('swipl_session')).
session_setting(path(/)).

session_option(timeout, integer).
session_option(cookie, atom).
session_option(path, atom).
session_option(route, atom).

%%	http_set_session_options(+Options) is det.
%
%	Set options for the session library.  Provided options are:
%	
%		* timeout(+Seconds)
%		Session timeout in seconds.  Default is 600 (10 min).
%		
%		* cookie(+Cookiekname)
%		Name to use for the cookie to identify the session.
%		Default =swipl_session=.
%		
%		* path(+Path)
%		Path to which the cookie is associated.  Default is
%		=|/|=.	Cookies are only sent if the HTTP request path
%		is a refinement of Path.
%
%		* route(+Route)
%		Set the route name. Default is the unqualified
%		hostname. To cancel adding a route, use the empty
%		atom.  See route/1.

http_set_session_options([]).
http_set_session_options([H|T]) :-
	http_session_option(H),
	http_set_session_options(T).

http_session_option(Option) :-
	functor(Option, Name, Arity),
	arg(1, Option, Value),
	(   session_option(Name, Type)
	->  must_be(Type, Value)
	;   domain_error(http_session_option, Option)
	),
	functor(Free, Name, Arity),
	retractall(session_setting(Free)),
	assert(session_setting(Option)).

%%	http_session_id(-SessionId) is det.
%	
%	True if SessionId is an identifier for the current session.
%	
%	@param SessionId is an atom.
%	@error existence_error(http_session, _)
%	@see   http_in_session/1 for a version that fails if there is
%	       no session.

http_session_id(SessionID) :-
	(   http_in_session(ID)
	->  SessionID = ID
	;   throw(error(existence_error(http_session, _), _))
	).

%%	http_in_session(-SessionId) is semidet.
%
%	True if SessionId is an identifier  for the current session. The
%	current session is extracted from   session(ID) from the current
%	HTTP request (see http_current_request/1). The   value is cached
%	in a backtrackable global variable   =http_session_id=.  Using a
%	backtrackable global variable is safe  because continuous worker
%	threads use a failure driven  look   and  spawned  threads start
%	without any global variables. This variable  can be set from the
%	commandline to fake running a goal   from the commandline in the
%	context of a session.
%	
%	@see http_session_id/1

http_in_session(SessionID) :-
	(   nb_current(http_session_id, ID),
	    ID \== []
	->  true
	;   http_current_request(Request),
	    memberchk(session(ID), Request),
	    b_setval(http_session_id, ID)
	;   b_setval(http_session_id, no_session),
	    fail
	),
	ID \== no_session,
	SessionID = ID.
	
%%	http_session(+RequestIn, -RequestOut, -SessionID) is semidet.
%	
%	Maintain the notion of a  session   using  a client-side cookie.
%	This must be called first when handling a request that wishes to
%	do session management, after which the possibly modified request
%	must be used for further processing.

http_session(Request, Request, SessionID) :-
	memberchk(session(SessionID0), Request), !,
	SessionID = SessionID0.
http_session(Request0, Request, SessionID) :-
	memberchk(cookie(Cookies), Request0),
	session_setting(cookie(Cookie)),
	memberchk(Cookie=SessionID0, Cookies),
	peer(Request0, Peer),
	valid_session_id(SessionID0, Peer), !,
	SessionID = SessionID0,
	Request = [session(SessionID)|Request0],
	b_setval(http_session_id, SessionID).
http_session(Request0, Request, SessionID) :-
	session_setting(path(Path)),
	memberchk(path(ReqPath), Request0),
	sub_atom(ReqPath, 0, _, _, Path), !,
	http_gc_sessions,		% GC dead sessions
	gen_cookie(SessionID),
	session_setting(cookie(Cookie)),
	format('Set-Cookie: ~w=~w; path=~w~n', [Cookie, SessionID, Path]),
	Request = [session(SessionID)|Request0],
	peer(Request0, Peer),
	open_session(SessionID, Peer),
	b_setval(http_session_id, SessionID).

:- multifile
	http:request_expansion/2.

http:request_expansion(Request0, Request) :-
	http_session(Request0, Request, _SessionID).

%%	peer(+Request, -Peer)
%
%	Find peer for current request. If   unknown we leave it unbound.
%	Alternatively we should treat this as an error.

peer(Request, Peer) :-
	(   memberchk(peer(Peer), Request)
	->  true
	;   true
	).

%%	open_session(+SessionID, +Peer)
%	
%	Open a new session.  Uses broadcast/1 with the term
%	http_session(begin(SessionID, Peer)).

open_session(SessionID, Peer) :-
	get_time(Now),
	assert(current_session(SessionID, Peer)),
	assert(last_used(SessionID, Now)),
	broadcast(http_session(begin(SessionID, Peer))).


%%	valid_session_id(+SessionID, +Peer)
%	
%	Check if this sessionID is known. If so, check the idle time and
%	update the last_used for this session.

valid_session_id(SessionID, Peer) :-
	current_session(SessionID, SessionPeer),
	get_time(Now),
	(   session_setting(timeout(Timeout)),
	    Timeout > 0
	->  get_last_used(SessionID, Last),
	    Idle is Now - Last,
	    (	Idle =< Timeout
	    ->  true
	    ;   close_session(SessionID),
		fail
	    )
	;   Peer \== SessionPeer
	->  close_session(SessionID),
	    fail
	;   true
	),
	set_last_used(SessionID, Now).

get_last_used(SessionID, Last) :-
	with_mutex(http_session, last_used(SessionID, Last)).

set_last_used(SessionID, Now) :-
	with_mutex(http_session,
		  (   retractall(last_used(SessionID, _)),
		      assert(last_used(SessionID, Now)))).



		 /*******************************
		 *	   SESSION DATA		*
		 *******************************/

%%	http_session_asserta(+Data) is det.
%%	http_session_assert(+Data) is det.
%%	http_session_retract(?Data) is nondet.
%%	http_session_retractall(?Data) is det.
%
%	Versions of assert/1, retract/1 and retractall/1 that associate
%	data with the current HTTP session.

http_session_asserta(Data) :-
	http_session_id(SessionId),
	asserta(session_data(SessionId, Data)).

http_session_assert(Data) :-
	http_session_id(SessionId),
	assert(session_data(SessionId, Data)).

http_session_retract(Data) :-
	http_session_id(SessionId),
	retract(session_data(SessionId, Data)).

http_session_retractall(Data) :-
	http_session_id(SessionId),
	retractall(session_data(SessionId, Data)).

%	http_session_data(?Data) is nondet.
%	
%	True if Data is associated using http_session_assert/1 to the
%	current HTTP session.

http_session_data(Data) :-
	http_session_id(SessionId),
	session_data(SessionId, Data).


		 /*******************************
		 *	     ENUMERATE		*
		 *******************************/

%%	http_current_session(?SessionID, ?Data) is nondet.
%	
%	Enumerate the current sessions and   associated data.  There are
%	two _Pseudo_ data elements:
%	
%		* idle(Seconds)
%		Session has been idle for Seconds.
%		
%		* peer(Peer)
%		Peer of the connection.

http_current_session(SessionID, Data) :-
	get_time(Now),
	get_last_used(SessionID, Last),
	Idle is Now - Last,
	(   session_setting(timeout(Timeout)),
	    Timeout > 0
	->  Idle =< Timeout
	;   true
	),
	(   Data = idle(Idle)
	;   Data = peer(Peer),
	    current_session(SessionID, Peer)
	;   session_data(SessionID, Data)
	).


		 /*******************************
		 *	    GC SESSIONS		*
		 *******************************/

%%	close_session(+SessionID)
%
%	Closes an HTTP session.   Broadcasts http_session(end(SessionId,
%	Peer)).

close_session(SessionId) :-
	(   retract(current_session(SessionId, Peer)),
	    broadcast(http_session(end(SessionId, Peer))),
	    retractall(last_used(SessionId, _)),
	    retractall(session_data(SessionId, _)),
	    fail
	;   true
	).

%	http_gc_sessions/0
%	
%	Delete dead sessions. When  should  we   be  calling  this? This
%	assumes that updated sessions are at the end of the clause list,
%	so we can break  as  soon   as  we  encounter  a no-yet-timedout
%	session.

http_gc_sessions :-
	session_setting(timeout(Timeout)),
	Timeout > 0, !,
	get_time(Now),
	(   last_used(SessionID, Last),
	    Idle is Now - Last,
	    (	Idle > Timeout
	    ->	close_session(SessionID),
		fail
	    ;	!
	    )
	;   true
	).
http_gc_sessions.


		 /*******************************
		 *	       UTIL		*
		 *******************************/

%%	gen_cookie(-Cookie) is det.
%	
%	Generate a random cookie that  can  be   used  by  a  browser to
%	identify  the  current  session.  The   cookie  has  the  format
%	XXXX-XXXX-XXXX-XXXX[.<route>], where XXXX are random hexadecimal
%	numbers  and  [.<route>]  is  the    optionally   added  routing
%	information.

gen_cookie(Cookie) :-
	route(Route), !,
	random_4(R1,R2,R3,R4),
	format(atom(Cookie),
		'~`0t~16r~4|-~`0t~16r~9|-~`0t~16r~14|-~`0t~16r~19|.~w',
		[R1,R2,R3,R4,Route]).
gen_cookie(Cookie) :-
	random_4(R1,R2,R3,R4),
	format(atom(Cookie),
		'~`0t~16r~4|-~`0t~16r~9|-~`0t~16r~14|-~`0t~16r~19|',
		[R1,R2,R3,R4]).

:- thread_local
	route_cache/1.

%%	route(-RouteID) is semidet.
%
%	Fetch the route identifier. This value   is added as .<route> to
%	the session cookie and used  by   -for  example- the apache load
%	balanching module. The default route is   the  local name of the
%	host.     Alternatives     may      be       provided      using
%	http_set_session_options/1.

route(Route) :-
	route_cache(Route), !,
	Route \== ''.
route(Route) :-
	route_no_cache(Route),
	assert(route_cache(Route)),
	Route \== ''.

route_no_cache(Route) :-
	session_setting(route(Route)), !.
route_no_cache(Route) :-
	gethostname(Host),
	(   sub_atom(Host, Before, _, _, '.')
	->  sub_atom(Host, 0, Before, _, Route)
	;   Route = Host
	).


%%	random_4(-R1,-R2,-R3,-R4) is det.
%
%	Generate 4 2-byte random  numbers.   Uses  =|/dev/urandom|= when
%	available to make prediction of the session IDs hard.

random_4(R1,R2,R3,R4) :-
	urandom(In), !,
	get_pair(In, R1),
	get_pair(In, R2),
	get_pair(In, R3),
	get_pair(In, R4).
random_4(R1,R2,R3,R4) :-
	R1 is random(65536),
	R2 is random(65536),
	R3 is random(65536),
	R4 is random(65536).

:- dynamic
	urandom_handle/1.

urandom(Handle) :-
	urandom_handle(Handle), !,
	Handle \== [].
urandom(Handle) :-
	catch(open('/dev/urandom', read, In, [type(binary)]), _, fail), !,
	assert(urandom_handle(In)),
	Handle = In.
urandom(_) :-
	assert(urandom_handle([])),
	fail.

get_pair(In, Value) :-
	get_byte(In, B1),
	get_byte(In, B2),
	Value is B1<<8+B2.
